{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Compile where

import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Fail hiding (fail)
import Control.Monad.Except hiding (void, fail)
import Foreign.Ptr

import LLVM.AST.Name hiding (Func)
import qualified LLVM.AST as LL
import qualified LLVM.AST.Type as LL
import qualified LLVM.AST.Constant as C
import qualified LLVM.Internal.FFI.DataLayout as FFI
import LLVM.AST.Global
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Context

import qualified AST as S
import qualified Flatten as F
import Monad
import Type
import Error
import Value
import State
import Print
import Funcs
import Table
import Tuple
import Construct
import Typeof
import Trace
import Interop
import ADT
import Array

funcKeyMatch :: [Type] -> [SymKey] -> [SymKey]
funcKeyMatch argTypes []         = []
funcKeyMatch argTypes (key:keys) = case key of
    KeyFunc ts rt
        | ts == argTypes -> KeyFunc ts rt : funcKeyMatch argTypes keys
    _ -> funcKeyMatch argTypes keys


compile :: BoM s m => [CompileState] -> S.AST ->  m ([LL.Definition], CompileState)
compile imports ast = do
    ((_, defs), state) <- runBoMTExcept (initCompileState imports modName) (runModuleCmpT emptyModuleBuilder cmp)
    return (defs, state)
    where
        modName = case S.astModuleName ast of
            Nothing   -> "main"
            Just name -> name

        cmp :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        cmp = do
            mainOp <- func (LL.mkName "main")  [] LL.VoidType $ \_ -> do
                mapM_ cmpTypeDef   [ stmt | stmt@(S.Typedef _ _ _) <- S.astStmts ast ]
                mapM_ cmpFuncHdr   [ stmt | stmt@(S.FuncDef _ _ _ _ _) <- S.astStmts ast ]
                mapM_ cmpVarDef    [ stmt | stmt@(S.Assign _ _ _) <- S.astStmts ast ]
                mapM_ cmpFuncDef   [ stmt | stmt@(S.FuncDef _ _ _ _ _) <- S.astStmts ast ]

            func (LL.mkName $ modName ++ "..callMain")  [] LL.VoidType $ \_ -> do
                boolName <- myFresh "bMainCalled"
                bMainCalled <- global boolName LL.i1 $ toCons (bit 0)
                b <- load bMainCalled 0

                true  <- freshName "main_called_true"
                exit  <- freshName "main_called_exit"

                condBr b true exit
                emitBlockStart true
                retVoid

                emitBlockStart exit
                store bMainCalled 0 (bit 1)

                forM_ (map curModName imports) $ \modName -> case modName of
                    "c" -> return ()
                    _ -> do
                        op <- extern (LL.mkName $ modName ++ "..callMain") [] LL.VoidType
                        void $ call op []

                void $ call mainOp []

            return ()


cmpTypeDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpTypeDef (S.Typedef pos symbol (S.AnnoTuple xs)) = withPos pos $ tupleTypeDef symbol (S.AnnoTuple xs)
cmpTypeDef (S.Typedef pos symbol (S.AnnoADT xs))   = withPos pos $ adtTypeDef symbol (S.AnnoADT xs)
cmpTypeDef (S.Typedef pos symbol (S.AnnoType typ)) = withPos pos $ do
    case typ of
        t | isTuple t -> tupleTypeDef symbol (S.AnnoType t)
        t             -> do
            let typdef = Typedef symbol
            define symbol (KeyFunc [] typdef) ObjConstructor
            define symbol (KeyFunc [t] typdef) ObjConstructor
            define symbol (KeyFunc [typdef] typdef) ObjConstructor
            define symbol KeyType (ObType t Nothing)
                    

cmpVarDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpVarDef (S.Assign pos (S.PatIdent p symbol) expr) = trace "cmpVarDef" $ withPos pos $ do
    val <- cmpExpr expr
    name <- myFresh (sym symbol)

    let typ = valType val
    opTyp <- opTypeOf typ

    if isCons (valOp val)
    then do
        loc <- Ptr typ <$> global name opTyp (toCons $ valOp val)
        define symbol KeyVar (ObjVal loc)
    else do
        initialiser <- valZero typ
        loc <- Ptr typ <$> global name opTyp (toCons $ valOp initialiser)
        valStore loc val
        define symbol KeyVar (ObjVal loc)

    addSymKeyDec symbol KeyVar name (DecVar opTyp)
    addDeclared name



cmpFuncHdr :: InsCmp CompileState m => S.Stmt -> m ()
cmpFuncHdr (S.FuncDef pos "main" params retty blk) = trace "cmpFuncHdr" $ return ()
cmpFuncHdr (S.FuncDef pos sym params retty blk)    = trace "cmpFuncHdr" $ withPos pos $ do
    let paramTypes = map S.paramType params
    name <- myFresh sym
    paramOpTypes <- mapM opTypeOf paramTypes
    returnOpType <- opTypeOf retty
    let op = fnOp name paramOpTypes returnOpType False

    define (Sym sym) (KeyFunc paramTypes retty) (ObjFunc op) 
    --redefine (Sym sym) KeyVar $ ObjVal $ Val (Func paramTypes retty) op

    addSymKeyDec (Sym sym) (KeyFunc paramTypes retty) name (DecFunc paramOpTypes returnOpType)
    addSymKeyDec (Sym sym) KeyVar name (DecFunc paramOpTypes returnOpType)

    addDeclared name


cmpFuncDef :: (MonadFail m, Monad m, MonadIO m) => S.Stmt -> InstrCmpT CompileState m ()
cmpFuncDef (S.FuncDef pos "main" params retty blk) = trace "cmpFuncDef" $ withPos pos $ do
    assert (params == [])  "main cannot have parameters"
    assert (retty == Void) $ "main must return void: " ++ show retty
    cmpStmt blk
cmpFuncDef (S.FuncDef pos sym params retty blk) = trace "cmpFuncDef" $ withPos pos $ do
    returnOpType <- opTypeOf retty
    paramOpTypes <- mapM (opTypeOf . S.paramType) params
    let paramTypes = map S.paramType params
    let paramSymbols = map S.paramName params
    let paramNames = map (ParameterName . mkBSS . Type.sym) paramSymbols

    ObjFunc op <- look (Sym sym) (KeyFunc paramTypes retty)
    let LL.ConstantOperand (C.GlobalReference _ name) = op
    let Name nameStr = name

    void $ InstrCmpT . IRBuilderT . lift $ func name (zip paramOpTypes paramNames) returnOpType $ \paramOps -> do
        forM_ (zip3 paramTypes paramOps paramSymbols) $ \(typ, op, symbol) -> do
            loc <- valLocal typ
            valStore loc (Val typ op)
            define symbol KeyVar (ObjVal loc)

        cmpStmt blk
        hasTerm <- hasTerminator
        if hasTerm
        then return ()
        else if retty == Void
        then retVoid
        else unreachable



cmpIndex:: InsCmp CompileState m => S.Index -> m Value
cmpIndex index = withPos index $ case index of
    S.IndIdent pos symbol -> do
        ObjVal val <- look symbol KeyVar
        return val
    S.IndArray pos ind idxExpr -> do
        idxVal <- cmpExpr idxExpr
        assertBaseType isInt (valType idxVal)

        loc <- cmpIndex ind
        base <- baseTypeOf (valType loc)
        case base of
            Table [t] -> do
                row <- tableRow 0 loc
                ptr <- valPtrIdx row idxVal
                return ptr
            Array n t -> do
                arrayGetElem loc idxVal


cmpInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
cmpInfix op valA valB = do
    kos <- lookSym (Sym $ show op)
    let matches = funcKeyMatch [valType valA, valType valB] (map fst kos)
    case matches of
        []              -> valsInfix op valA valB
        [KeyFunc ts rt] -> do
            ObjFunc op <- look (Sym $ show op) $ KeyFunc ts rt
            opA <- valOp <$> valLoad valA
            opB <- valOp <$> valLoad valB
            Val rt <$> call op [(opA, []), (opB, [])]


cmpCondition :: InsCmp CompileState m => S.Condition -> m Value
cmpCondition cnd = trace "cmpCondition" $ do
    val <- case cnd of
        S.CondExpr expr      -> cmpExpr expr
        S.CondMatch pat expr -> cmpPattern pat =<< cmpExpr expr

    assertBaseType (== Bool) (valType val)
    return val


cmpPrint :: InsCmp CompileState m => S.Stmt -> m ()
cmpPrint (S.Print pos exprs) = trace "cmpPrint" $ do
    prints =<< mapM cmpExpr exprs
    where
        prints :: InsCmp CompileState m => [Value] -> m ()
        prints []     = void $ printf "\n" []
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs


cmpAppend :: InsCmp CompileState m => S.Append -> m Value
cmpAppend append = withPos append $ case append of
    S.AppendIndex index -> cmpIndex index

    S.AppendTable pos app expr -> do
        loc <- cmpAppend app
        tableAppend loc =<< cmpExpr expr
        return loc


cmpStmt :: InsCmp CompileState m => S.Stmt -> m ()
cmpStmt stmt = trace "cmpStmt" $ withPos stmt $ case stmt of
    S.Print pos exprs   -> cmpPrint stmt
    S.Block stmts       -> mapM_ cmpStmt stmts
    S.AppendStmt append -> void $ cmpAppend append

    S.CallStmt pos symbol exprs -> do
        vals <- mapM cmpExpr exprs

        kos <- lookSym symbol
        case funcKeyMatch (map valType vals) (map fst kos) of
            [] -> fail $ "no matching function"
            [KeyFunc ts rt] -> do
                ObjFunc op <- look symbol $ KeyFunc ts rt
                vals' <- mapM valLoad vals
                void $ call op [(o, []) | o <- map valOp vals']

    S.Assign pos pat expr -> trace ("assign " ++ show pat) $ do
        matched <- valLoad =<< cmpPattern pat =<< cmpExpr expr
        if_ (valOp matched) (return ()) (void trap) 

    S.Set pos ind expr -> do
        loc <- cmpIndex ind
        valStore loc =<< cmpExpr expr

    S.Return pos Nothing -> do
        retVoid
        emitBlockStart =<< fresh

    S.Return pos (Just expr) -> do
        ret . valOp =<< valLoad =<< cmpExpr expr
        emitBlockStart =<< fresh

    S.If pos cnd blk melse -> do
        val <- valLoad =<< cmpCondition cnd
        assertBaseType (== Bool) (valType val)
        if_ (valOp val) (cmpStmt blk) $ maybe (return ()) cmpStmt melse

    S.While pos cnd blk -> do
        cond <- freshName "while_cond"
        body <- freshName "while_body"
        exit <- freshName "while_exit"

        br cond
        emitBlockStart cond
        val <- valLoad =<< cmpCondition cnd
        condBr (valOp val) body exit
        
        emitBlockStart body
        cmpStmt blk
        br cond
        emitBlockStart exit

    S.Typedef _ _ _ -> cmpTypeDef stmt
    
    S.Switch _ expr cases -> do
        val <- cmpExpr expr
        let cases' = [(fmap valOp (cmpPattern pat val), cmpStmt stmt) | (pat, stmt) <- cases]
        switch_ $ cases' ++ [(return (bit 1), void trap)]
    
    S.For _ symbol (Just typ) mexpr mcnd blk -> do
        idx <- valLocal typ
        valStore idx =<< valZero typ
        define symbol KeyVar (ObjVal idx) 

        mval <- case mexpr of
            Nothing -> return Nothing
            Just expr -> Just <$> cmpExpr expr


        cond <- freshName "for_cond"
        body <- freshName "for_body"
        exit <- freshName "for_exit"

        br cond
        emitBlockStart cond

        ilt <- case mval of
            Nothing -> valBool Bool True
            Just val -> do
                base <- baseTypeOf (valType val)
                len <- case base of
                    Table ts  -> tableLen val
                    Array n t -> return $ valI64 n
                valsInfix S.LT idx len

        cnd <- case mcnd of
            Nothing -> return ilt
            Just gc -> valsInfix S.AndAnd ilt =<< cmpCondition gc

        condBr (valOp cnd) body exit
        
        emitBlockStart body
        cmpStmt blk

        valStore idx =<< valsInfix S.Plus idx =<< valInt typ 1
        br cond
        emitBlockStart exit
        

    _ -> error "stmt"


-- must return Val unless local variable
cmpExpr :: InsCmp CompileState m =>  S.Expr -> m Value
cmpExpr (S.AExpr exprType expr) = trace "cmpExpr" $ withPos expr $ withCheck exprType $ case expr of
    S.Bool pos b               -> valBool exprType b
    S.Char pos c               -> valChar exprType c
    S.Conv pos typ exprs       -> valConstruct typ =<< mapM cmpExpr exprs
    S.Copy pos expr            -> valCopy =<< cmpExpr expr
    S.Tuple pos [expr]         -> cmpExpr expr
    S.Float p f                -> valFloat exprType f

    S.Table p [[]] -> do
        assertBaseType isTable exprType
        valZero exprType

    S.Int p n -> do
        base <- baseTypeOf exprType
        case base of
            _ | isInt base   -> valInt exprType n
            _ | isFloat base -> valFloat exprType (fromIntegral n)
            _ | base == Char -> valChar exprType (chr $ fromIntegral n)
            _ -> fail $ "invalid base type: " ++ show base

    S.Infix pos op exprA exprB -> do
        valA <- cmpExpr exprA
        valB <- cmpExpr exprB
        cmpInfix op valA valB

    S.Ident pos symbol -> do
        obj <- look symbol KeyVar
        case obj of
            ObjVal (ConstInt n) -> valInt I64 n
            ObjVal loc -> return loc

    S.Prefix pos S.Not   expr  -> valNot =<< cmpExpr expr
    S.Prefix pos S.Minus expr  -> do
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)
        x <- case base of
            I64 -> valInt (valType val) 0
            F64 -> valFloat (valType val) 0

        valsInfix S.Minus x val
            
    S.String pos s -> do
        Table ts <- assertBaseType isTable exprType
        assert (length ts == 1) "Cannot compile string as type"
        assertBaseType (== Char) (head ts)

        loc <- globalStringPtr s =<< myFresh "str"
        let pi8 = C.BitCast loc (LL.ptr LL.i8)
        let i64 = toCons $ int64 $ fromIntegral (length s)
        let stc = struct Nothing False [i64, i64, pi8]
        return (Val exprType stc)

    S.Call pos symbol exprs -> do
        vals <- mapM valLoad =<< mapM cmpExpr exprs
        obj <- look symbol $ KeyFunc (map valType vals) exprType
        case obj of
            ObjConstructor     -> valConstruct exprType vals
            ObjFunc op         -> Val exprType <$> call op [(o, []) | o <- map valOp vals]
            ObjADTFieldCons    -> adtConstructField symbol exprType vals

    S.Len pos expr -> valLoad =<< do
        assertBaseType isIntegral exprType
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)
        case base of
            Table _   -> valConvertNumber exprType =<< tableLen val
            Array n t -> valInt exprType n
            _       -> fail ("cannot take length of type " ++ show (valType val))

    S.Tuple pos exprs -> do
        Tuple ts <- assertBaseType isTuple exprType
        vals <- mapM cmpExpr exprs
        assert (ts == map valType vals) "Incorrect val types."
        tup <- valLocal exprType
        zipWithM_ (tupleSet tup) [0..] vals
        return tup

    S.Range pos expr mexpr1 mexpr2 -> do
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)
        case base of
            Table ts -> do
                start <- maybe (valInt I64 0) cmpExpr mexpr1
                end <- maybe (tableLen val) cmpExpr mexpr2
                tableRange val start end

    S.Subscript pos aggExpr idxExpr -> do
        agg <- cmpExpr aggExpr
        idx <- cmpExpr idxExpr

        idxType <- assertBaseType isInt (valType idx)
        aggType <- baseTypeOf (valType agg)

        case aggType of
            Table [t] -> tableGetElem agg idx
            Array n t -> arrayGetElem agg idx

    S.Table pos exprss -> valLoad =<< do
        base <- baseTypeOf exprType
        case base of
            Array n t -> do
                assert (length exprss == 1) "Arrays cannot have multiple rows"
                let exprs = head exprss
                vals <- mapM cmpExpr exprs
                arrayMake exprType
                
            Table ts -> do
                valss <- mapM (mapM cmpExpr) exprss
                assert (length valss > 0) "Cannot infer type of table."
                let rowLen = length (head valss)
                assert (rowLen > 0) "Cannot infer type of table."

                forM_ valss $ \vals -> do
                    assert (length vals == rowLen) $
                        "Mismatched table row length of " ++ show (length vals)

                tab <- tableMake exprType (valI64 rowLen)
                
                forM (zip ts [0..]) $ \(t, r) ->
                    forM [0..rowLen - 1] $ \i -> do
                        row <- tableRow r tab
                        ptr <- valPtrIdx row (valI64 i)
                        valStore ptr $ (valss !! r) !! i

                return tab

    S.Member pos exp sym -> do
        tupleMember sym =<< cmpExpr exp 

    _ -> fail ("invalid expression: " ++ show expr)
    where
        withCheck :: InsCmp s m => Type -> m Value -> m Value
        withCheck typ m = do
            val <- m
            assert (valType val == typ) $ "Expression compiled to: " ++ show (valType val) ++ " instead of: " ++ show typ
            return val
            


cmpPattern :: InsCmp CompileState m => S.Pattern -> Value -> m Value
cmpPattern pattern val = trace "cmpPattern" $ withPos pattern $ case pattern of
    S.PatIgnore _   -> valBool Bool True
    S.PatLiteral expr -> cmpInfix S.EqEq val =<< cmpExpr expr

    S.PatGuarded _ pat expr -> do
        match <- cmpPattern pat =<< valLoad val
        guard <- cmpExpr expr
        assertBaseType (== Bool) (valType guard)
        valsInfix S.AndAnd match guard

    S.PatIdent _ symbol -> trace ("cmpPattern " ++ show pattern) $ do
        base <- baseTypeOf (valType val)
        loc <- valLocal (valType val)
        valStore loc val
        define symbol KeyVar (ObjVal loc)
        valBool Bool True

    S.PatTuple _ pats -> do
        len <- tupleLength val
        assert (len == length pats) "tuple pattern length mismatch"

        bs <- forM (zip pats [0..]) $ \(p, i) ->
            cmpPattern p =<< tupleIdx i val

        true <- valBool Bool True
        foldM (valsInfix S.AndAnd) true bs

    S.PatArray _ pats -> do
        base <- baseTypeOf (valType val)
        case base of
            Table ts -> do
                len   <- tableLen val
                lenEq <- valsInfix S.EqEq len (valI64 $ length pats)

                assert (length ts == 1) "patterns don't support multiple rows (yet)"
                bs <- forM (zip pats [0..]) $ \(p, i) ->
                    cmpPattern p =<< tableGetElem val (valI64 i)

                true <- valBool (valType lenEq) True
                foldM (valsInfix S.AndAnd) true (lenEq:bs)
            _ -> fail "Invalid array pattern"

    S.PatField _ symbol pats -> do
        base@(ADT tss) <- assertBaseType isADT (valType val)
        ObjMember i <- look symbol (KeyMember $ valType val)
        case base of
            _ | isEnumADT base -> do
                assert (length pats == 0) "invalid ADT pattern"
                valsInfix S.EqEq (valI64 i) =<< adtEnum val

            _ | isNormalADT base -> do
                let ts = tss !! i
                assert (length pats == length ts) "invalid ADT pattern"
                valBool Bool True
                bs <- forM (zip pats [0..]) $ \(pat, ti) -> 
                    cmpPattern pat =<< adtDeref val i ti
                enumMatch <- valsInfix S.EqEq (valI64 i) =<< adtEnum val
                foldM (valsInfix S.AndAnd) enumMatch bs


