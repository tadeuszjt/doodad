{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Compile where

import Data.List
import Data.Maybe
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
import ADT
import Construct
import Typeof
import Trace

compileFlatState :: BoM s m
    => Map.Map S.ModuleName CompileState
    -> F.FlattenState
    -> S.ModuleName
    -> m ([LL.Definition], CompileState)
compileFlatState imports flatState modName = do
    res <- runBoMT (initCompileState imports modName) (runModuleCmpT emptyModuleBuilder cmp)
    case res of
        Left err                 -> throwError err
        Right ((_, defs), state) -> return (defs, state)
    where
        cmp :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        cmp = do
            mainName <- case modName of
                "main" -> return "main"
                s      -> return (s ++ "__main")

            void $ func (LL.mkName mainName)  [] LL.VoidType $ \_ -> do
                cmpMainGuard

                forM_ (Map.toList $ F.typeDefs flatState) $ \(sym, (pos, typ)) ->
                    withPos pos (cmpTypeDef sym typ)

                mapM_ cmpFuncHdr (F.funcDefs flatState)
                mapM_ cmpExternDef (F.externDefs flatState)
                mapM_ cmpVarDef (F.varDefs flatState)
                mapM_ cmpFuncDef (F.funcDefs flatState)
        
        cmpMainGuard :: InsCmp CompileState m => m ()
        cmpMainGuard = do
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

            forM_ (Map.keys imports) $ \modName -> do
                op <- extern (LL.mkName $ modName ++ "__main") [] LL.VoidType
                call op []


cmpTypeDef :: InsCmp CompileState m => String -> Type -> m ()
cmpTypeDef sym typ = trace "cmpTypeDef" $ do
    case typ of
        t | isADT t   -> adtTypeDef sym t
        t | isTuple t -> tupleTypeDef sym t
        t             -> do
            let typdef = Typedef (Sym sym)
            addObjWithCheck sym (KeyFunc []) (ObjConstructor typdef)
            addObjWithCheck sym (KeyFunc [t]) (ObjConstructor typdef)
            addObjWithCheck sym (KeyFunc [typdef]) (ObjConstructor typdef)
            addObjWithCheck sym KeyType (ObType t Nothing)
                    

cmpVarDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpVarDef (S.Assign pos (S.PatIdent p sym) expr) = trace "cmpVarDef" $ withPos pos $ do
    val <- cmpExpr expr
    name <- myFresh sym

    let typ = valType val
    opTyp <- opTypeOf typ

    if isCons (valOp val)
    then do
        loc <- Ptr typ <$> global name opTyp (toCons $ valOp val)
        addObjWithCheck sym KeyVar (ObjVal loc)
    else do
        initialiser <- valZero typ
        loc <- Ptr typ <$> global name opTyp (toCons $ valOp initialiser)
        valStore loc val
        addObjWithCheck sym KeyVar (ObjVal loc)

    addSymKeyDec sym KeyVar name (DecVar opTyp)
    addDeclared name


cmpExternDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpExternDef (S.Extern pos nameStr sym params retty) = trace "cmpExternDef" $ withPos pos $ do
    checkSymUndef sym 
    let name = LL.mkName nameStr
    let paramTypes = map S.paramType params

    pushSymTab
    paramOpTypes <- forM params $ \(S.Param p s t) -> do
        addObjWithCheck s KeyVar $ ObjVal (valBool False)
        opTypeOf t

    returnOpType <- opTypeOf retty
    popSymTab

    addSymKeyDec sym (KeyFunc paramTypes) name (DecExtern paramOpTypes returnOpType False)
    let op = fnOp name paramOpTypes returnOpType False
    addObj sym (KeyFunc paramTypes) (ObjExtern paramTypes retty op)


cmpFuncHdr :: InsCmp CompileState m => S.Stmt -> m ()
cmpFuncHdr (S.FuncDef pos "main" params retty blk) = trace "cmpFuncHdr" $ return ()
cmpFuncHdr (S.FuncDef pos sym params retty blk)    = trace "cmpFuncHdr" $ withPos pos $ do
    let paramTypes = map S.paramType params
    name <- myFresh sym
    paramOpTypes <- mapM opTypeOf paramTypes
    returnOpType <- opTypeOf retty
    let op = fnOp name paramOpTypes returnOpType False

    addObjWithCheck sym (KeyFunc paramTypes) (ObjFunc retty op) 
    addObj sym KeyVar $ ObjVal $ Val (Func paramTypes retty) op

    addSymKeyDec sym (KeyFunc paramTypes) name (DecFunc paramOpTypes returnOpType)
    addSymKeyDec sym KeyVar name (DecFunc paramOpTypes returnOpType)

    addDeclared name


cmpFuncDef :: (MonadFail m, Monad m, MonadIO m) => S.Stmt -> InstrCmpT CompileState m ()
cmpFuncDef (S.FuncDef pos "main" params retty blk) = trace "cmpFuncDef" $ withPos pos $ do
    assert (params == [])  "main cannot have parameters"
    assert (retty == Void) "main must return void"
    cmpStmt blk
cmpFuncDef (S.FuncDef pos sym params retty blk) = trace "cmpFuncDef" $ withPos pos $ do
    returnOpType <- opTypeOf retty
    paramOpTypes <- mapM (opTypeOf . S.paramType) params
    let paramTypes = map S.paramType params
    let paramNames = map (ParameterName . mkBSS . S.paramName) params
    let paramSyms  = map S.paramName params

    ObjFunc _ op <- look (Sym sym) (KeyFunc paramTypes)
    let LL.ConstantOperand (C.GlobalReference _ name) = op
    let Name nameStr = name

    pushSymTab
    curRetty <- gets curRetType
    modify $ \s -> s { curRetType = retty }
    void $ InstrCmpT . IRBuilderT . lift $ func name (zip paramOpTypes paramNames) returnOpType $ \paramOps -> do
        forM_ (zip3 paramTypes paramOps paramSyms) $ \(typ, op, sym) -> do
            loc <- valLocal typ
            valStore loc (Val typ op)
            addObjWithCheck sym KeyVar (ObjVal loc)

        cmpStmt blk
        hasTerm <- hasTerminator
        retty <- gets curRetType
        if hasTerm
        then return ()
        else if retty == Void
        then retVoid
        else unreachable

    modify $ \s -> s { curRetType = curRetty }
    popSymTab


cmpIndex:: InsCmp CompileState m => S.Index -> m Value
cmpIndex index = case index of
    S.IndIdent pos sym -> withPos pos $ do
        ObjVal val <- look (Sym sym) KeyVar
        return val
    S.IndArray pos ind idxExpr -> do
        idxVal <- withTypeHint I64 (cmpExpr idxExpr)
        assertBaseType isInt (valType idxVal)

        loc <- cmpIndex ind
        base <- baseTypeOf (valType loc)
        case base of
            Table [t] -> do
                row <- tableRow 0 loc
                ptr <- valPtrIdx row idxVal
                return ptr



cmpInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
cmpInfix op valA valB = do
    resm <- lookm (Sym $ show op) $ KeyFunc [valType valA, valType valB]
    case resm of
        Just (ObjFunc Void op)   -> err "Operator function does not return a value."

        Just (ObjFunc retty op)  -> do
            opA <- valOp <$> valLoad valA
            opB <- valOp <$> valLoad valB
            Val retty <$> call op [(opA, []), (opB, [])]

        Nothing -> valsInfix op valA valB


cmpCondition :: InsCmp CompileState m => S.Condition -> m Value
cmpCondition cnd = trace "cmpCondition" $ do
    val <- case cnd of
        S.CondExpr expr      -> cmpExpr expr
        S.CondMatch pat expr -> cmpPattern pat =<< cmpExpr expr

    assertBaseType (== Bool) (valType val)
    return val


cmpPrint :: InsCmp CompileState m => S.Stmt -> m ()
cmpPrint (S.Print pos exprs) = trace "cmpPrint" $ withPos pos $ do
    prints =<< mapM cmpExpr exprs
    where
        prints :: InsCmp CompileState m => [Value] -> m ()
        prints []     = void $ printf "\n" []
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs


cmpAppend :: InsCmp CompileState m => S.Append -> m Value
cmpAppend append = case append of
    S.AppendIndex index -> cmpIndex index

    S.AppendTable pos app expr -> withPos pos $ do
        loc <- cmpAppend app
        val <- withTypeHint (valType loc) (cmpExpr expr)
        tableAppend loc val
        return loc

    S.AppendElem pos app expr -> withPos pos $ do
        loc <- cmpAppend app
        val <- cmpExpr expr
        tableAppendElem loc val
        return loc


cmpStmt :: InsCmp CompileState m => S.Stmt -> m ()
cmpStmt stmt = trace "cmpStmt" $ case stmt of
    S.Print pos exprs   -> cmpPrint stmt
    S.Block stmts       -> pushSymTab >> mapM_ cmpStmt stmts >> popSymTab
    S.AppendStmt append -> void $ cmpAppend append

    S.CallStmt pos (S.IndIdent _ sym) exprs -> withPos pos $ do
        vals <- mapM (valLoad <=< cmpExpr) exprs
        resm <- lookm (Sym sym) $ KeyFunc (map valType vals)
        op <- case resm of
            Just (ObjFunc _ op)     -> return op
            Just (ObjExtern _ _ op) -> return op
            Nothing                 -> do
                ObjVal fval <- look (Sym sym) KeyVar
                ftyp@(Func ts rt) <- assertBaseType isFunction (valType fval)
                assert (ts == map valType vals) ("Incorrect argument types for: " ++ show ftyp)
                valOp <$> valLoad fval

        void $ call op [(o, []) | o <- map valOp vals]

    S.CallStmt pos index exprs -> withPos pos $ do
        vals <- mapM (valLoad <=< cmpExpr) exprs
        fval <- valLoad =<< cmpIndex index
        ftyp@(Func ts rt) <- assertBaseType isFunction (valType fval)
        assert (ts == map valType vals) ("Incorrect argument types for: " ++ show ftyp)
        void $ call (valOp fval) [(o, []) | o <- map valOp vals]

    S.Assign pos pat expr -> withPos pos $ trace ("assign " ++ show pat) $ do
        matched <- valLoad =<< cmpPattern pat =<< cmpExpr expr
        if_ (valOp matched) (return ()) (void trap) 

    S.Set pos ind expr -> withPos pos $ do
        loc <- cmpIndex ind
        valStore loc =<< cmpExpr expr

    S.Return pos Nothing -> withPos pos $ do
        curRetty <- gets curRetType
        assert (curRetty == Void) "must return a value"
        retVoid
        emitBlockStart =<< fresh

    S.Return pos (Just expr) -> withPos pos $ do
        retty <- gets curRetType
        val <- withTypeHint retty (cmpExpr expr)
        checkTypesCompatible (valType val) retty
        ret . valOp =<< valLoad val
        emitBlockStart =<< fresh

    S.If pos cnd blk melse -> withPos pos $ do
        pushSymTab
        val <- valLoad =<< cmpCondition cnd
        assertBaseType (== Bool) (valType val)
        if_ (valOp val) (cmpStmt blk) $ maybe (return ()) cmpStmt melse
        popSymTab

    S.While pos cnd blk -> withPos pos $ do
        cond <- freshName "while_cond"
        body <- freshName "while_body"
        exit <- freshName "while_exit"

        pushSymTab
        br cond
        emitBlockStart cond
        val <- valLoad =<< cmpCondition cnd
        condBr (valOp val) body exit
        
        emitBlockStart body
        cmpStmt blk
        br cond
        emitBlockStart exit
        popSymTab

    S.For pos idxSym expr guardm blk -> withPos pos $ do
        val <- cmpExpr expr
        assertBaseType isTable (valType val)

        idx <- valLocal I64
        valStore idx (valI64 0)

        pushSymTab
        addObjWithCheck idxSym KeyVar (ObjVal idx)

        cond <- freshName "for_cond"
        body <- freshName "for_body"
        exit <- freshName "for_exit"

        br cond
        emitBlockStart cond
        cnd <- valLocal Bool
        valStore cnd =<< valsInfix S.LT idx =<< tableLen val
        when (isJust guardm) $ do
            cndOp <- valOp <$> valLoad cnd
            if_ cndOp
                (valStore cnd =<< cmpExpr (fromJust guardm))
                (return ())

        cndOp <- valOp <$> valLoad cnd
        condBr cndOp body exit

        emitBlockStart body
        cmpStmt blk
        valStore idx =<< valsInfix S.Plus idx (valI64 1)
        br cond
        emitBlockStart exit

        popSymTab

    S.Switch pos expr cases -> withPos pos $ do
        val <- cmpExpr expr

        exitName <- freshName "switch_exit"
        trapName <- freshName "switch_trap"
        cndNames <- replicateM (length cases) (freshName "case")
        stmtNames <- replicateM (length cases) (freshName "case_stmt")
        let nextNames = cndNames ++ [trapName]

        br (head nextNames)
        forM_ (zip4 cases cndNames stmtNames (tail nextNames)) $
            \((pat, stmt), cndName, stmtName, nextName) -> do
                emitBlockStart cndName
                pushSymTab
                matched <- valLoad =<< cmpPattern pat val
                condBr (valOp matched) stmtName nextName
                emitBlockStart stmtName
                cmpStmt stmt
                popSymTab
                br exitName

        emitBlockStart trapName
        void trap 
        br exitName
        emitBlockStart exitName

    _ -> err "stmt"


-- must return Val unless local variable
cmpExpr :: InsCmp CompileState m =>  S.Expr -> m Value
cmpExpr expr = trace "cmpExpr" $ case expr of
    S.Bool pos b               -> return (valBool b)
    S.Char pos c               -> return (valChar c)
    S.Conv pos typ [S.Null p]  -> withPos pos (adtNull typ)
    S.Conv pos typ exprs       -> withPos pos $ valConstruct typ =<< mapM cmpExpr exprs
    S.Copy pos expr            -> withPos pos $ valCopy =<< cmpExpr expr

    S.Float p f -> withPos p $ do
        hint <- gets typeHint
        base <- baseTypeOf hint
        if isFloat base
        then valFloat hint f
        else valFloat F64 f

    S.Table p [[]] -> withPos p $ do
        hint <- gets typeHint
        base <- baseTypeOf hint
        if isTable base
        then valZero hint
        else err "Table no type"

    S.Null p -> withPos p $ do
        hint <- gets typeHint
        base <- baseTypeOf hint
        if isADT base
        then adtNull hint
        else adtNull $ ADT [("", Void)]

    S.Int p n -> withPos p $ do
        hint <- gets typeHint
        base <- baseTypeOf hint
        if isInt base
        then valInt hint n
        else valInt I64 n

    S.Infix pos op exprA exprB -> withPos pos $ do
        valA <- cmpExpr exprA
        valB <- cmpExpr exprB
        cmpInfix op valA valB

    S.Ident symbol             -> do
        obj <- look symbol KeyVar
        case obj of
            ObjVal loc             -> return loc
            ObjADTFieldCons adtTyp -> adtConstructField (sym symbol) adtTyp []

    S.Prefix pos S.Not   expr  -> withPos pos $ valNot =<< cmpExpr expr
    S.Prefix pos S.Minus expr  -> withPos pos $ do
        val <- cmpExpr expr
        a <- valInt (valType val) 0
        valsInfix S.Minus a val
            
    S.String pos s -> do
        loc <- globalStringPtr s =<< myFresh "str"
        let pi8 = C.BitCast loc (LL.ptr LL.i8)
        let i64 = toCons $ int64 $ fromIntegral (length s)
        let stc = struct Nothing False [i64, i64, pi8]
        return $ Val (Table [Char]) stc

    S.Call pos expr exprs -> withPos pos $ trace "call" $ do
        vals <- mapM valLoad =<< mapM cmpExpr exprs

        (s, obj) <- case expr of
            S.Ident symbol               -> fmap (sym symbol,) $ look symbol $ KeyFunc (map valType vals)
            _                            -> fmap ("",) $ fmap ObjVal $ valLoad =<< cmpExpr expr
        
        case obj of
            ObjFunc Void _         -> err "cannot use void function as expression"
            ObjExtern _ Void _     -> err "cannot use void function as expression"
            ObjConstructor typ     -> valConstruct typ vals
            ObjADTFieldCons adtTyp -> adtConstructField s adtTyp vals
            ObjFunc retty op       -> Val retty <$> call op [(o, []) | o <- map valOp vals]
            ObjExtern _ retty op   -> Val retty <$> call op [(o, []) | o <- map valOp vals]
            ObjVal (Val typ op)    -> do
                Func ts rt <- assertBaseType isFunction typ
                assert (map valType vals == ts) ("Argument types do not match " ++ show ts)
                Val rt <$> call op [(o, []) | o <- map valOp vals]

    S.Len pos expr -> withPos pos $ valLoad =<< do
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)
        case base of
            Table _ -> tableLen val
            _       -> err ("cannot take length of type " ++ show (valType val))

    S.Tuple pos [expr] -> withPos pos (cmpExpr expr)

    S.Tuple pos exprs -> withPos pos $ do
        hint <- gets typeHint
        base <- baseTypeOf hint

        vals <- if isTuple base && (let Tuple ts = base in length ts == length exprs)
        then do
            let Tuple xs = base
            forM (zip xs exprs) $ \((s, t), e) -> withTypeHint t (cmpExpr e)
        else mapM cmpExpr exprs

        tup <- valLocal $ Tuple [ ("", valType v) | v <- vals ]
        zipWithM_ (tupleSet tup) [0..] vals
        valLoad tup

    S.Subscript pos aggExpr idxExpr -> withPos pos $ valLoad =<< do
        agg <- cmpExpr aggExpr
        idx <- withTypeHint I64 (cmpExpr idxExpr)

        idxType <- assertBaseType isInt (valType idx)
        aggType <- baseTypeOf (valType agg)

        case aggType of
            Table [t] -> tableGetElem agg idx

    S.Range pos expr mstart mend -> withPos pos $ do
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)
        case base of
            Table ts -> do
                start <- maybe (return $ valI64 0) cmpExpr mstart
                valLoad =<< tableRange val start =<< maybe (tableLen val) cmpExpr mend
    
    S.Table pos exprss -> withPos pos $ valLoad =<< do
        valss <- mapM (mapM cmpExpr) exprss
        assert (length valss > 0) "Cannot infer type of table."
        let rowLen = length (head valss)
        assert (rowLen > 0) "Cannot infer type of table."

        rowTypes <- forM valss $ \vals -> do
            assert (length vals == rowLen) $ "Mismatched table row length of " ++ show (length vals)
            let typ = valType (head vals)
            mapM_ (checkTypesCompatible typ) (map valType vals)
            return typ

        tab <- tableMake (Table rowTypes) (valI64 rowLen)
        
        forM (zip rowTypes [0..]) $ \(t, r) ->
            forM [0..rowLen - 1] $ \i -> do
                row <- tableRow r tab
                ptr <- valPtrIdx row (valI64 i)
                valStore ptr $ (valss !! r) !! i

        return tab

    S.Member pos exp sym -> withPos pos $ do
        tupleMember sym =<< cmpExpr exp 

    _ -> err ("invalid expression: " ++ show expr)


cmpPattern :: InsCmp CompileState m => S.Pattern -> Value -> m Value
cmpPattern pattern val = trace "cmpPattern" $ case pattern of
    S.PatIgnore pos -> return (valBool True)

    S.PatLiteral (S.Null pos) -> withPos pos $ trace "cmpPattern null" $ do
        ADT xs <- assertBaseType isADT (valType val)
        
        idx <- case [ i | (("", Void), i) <- zip xs [0..] ] of
            [i] -> return i
            _   -> err "ADT does not support a unique null field"

        valsInfix S.EqEq (valI64 idx) =<< adtEnum val

    S.PatLiteral expr -> cmpInfix S.EqEq val =<< cmpExpr expr

    S.PatGuarded pos pat expr -> withPos pos $ do
        match <- cmpPattern pat =<< valLoad val
        guard <- cmpExpr expr
        assertBaseType (== Bool) (valType guard)
        valsInfix S.AndAnd match guard

    S.PatIdent pos sym -> withPos pos $ trace ("cmpPattern " ++ show pattern) $ do
        base <- baseTypeOf (valType val)

        if isADT base && (let ADT xs = base in sym `elem` map fst xs)
        then do
            let ADT xs = base
            let idx = fromJust $ elemIndex sym (map fst xs)
            let (s, t) = xs !! idx
            assert (t == Void) "Ident patterns only valid for null fields"
            valsInfix S.EqEq (valI64 idx) =<< adtEnum val
        else do
            loc <- valLocal (valType val)
            valStore loc val
            addObjWithCheck sym KeyVar (ObjVal loc)
            return (valBool True)

    S.PatTuple pos pats -> withPos pos $ do
        len <- tupleLength val
        assert (len == length pats) "tuple pattern length mismatch"

        bs <- forM (zip pats [0..]) $ \(p, i) ->
            cmpPattern p =<< tupleIdx i val

        foldM (valsInfix S.AndAnd) (valBool True) bs

    S.PatArray pos pats -> withPos pos $ do
        base <- baseTypeOf (valType val)
        case base of
            Table ts -> do
                len   <- tableLen val
                lenEq <- valsInfix S.EqEq len (valI64 $ length pats)

                assert (length ts == 1) "patterns don't support multiple rows (yet)"
                bs <- forM (zip pats [0..]) $ \(p, i) ->
                    cmpPattern p =<< tableGetElem val (valI64 i)

                foldM (valsInfix S.AndAnd) (valBool True) (lenEq:bs)
            _ -> err "Invalid array pattern"

    S.PatSplit pos pat@(S.PatArray p pats) rest -> withPos pos $ do
        initMatched <- cmpPattern pat =<< tableRange val (valI64 0) (valI64 $ length pats)
        matched <- valLocal Bool
        if_ (valOp initMatched)
            (valStore matched =<< cmpPattern rest =<< tableRange val (valI64 $ length pats) =<< tableLen val)
            (valStore matched $ valBool False)
        valLoad matched

    S.PatSplit pos (S.PatLiteral (S.String p s)) rest -> withPos pos $ do
        let charPats = map (S.PatLiteral . S.Char p) s
        let arrPat   = S.PatArray p charPats
        cmpPattern (S.PatSplit pos arrPat rest) val

    S.PatSplitElem pos pat rest -> withPos pos $ do
        hasElem <- valsInfix S.LT (valI64 0) =<< tableLen val
        initMatched <- valLocal Bool
        if_ (valOp hasElem)
            (valStore initMatched =<< cmpPattern pat =<< tableGetElem val (valI64 0))
            (valStore initMatched $ valBool False)

        initMatchedVal <- valLoad initMatched
        matched <- valLocal Bool
        if_ (valOp initMatchedVal)
            (valStore matched =<< cmpPattern rest =<< tableRange val (valI64 1) =<< tableLen val)
            (valStore matched $ valBool False)

        valLoad matched

    S.PatTyped pos typ pats -> withPos pos $ do
        ADT xs <- assertBaseType isADT (valType val)

        let fieldMatched = \(s, t) -> (s == "" && t == typ) || Typedef (Sym s) == typ

        idx <- case [ i | (x, i) <- zip xs [0..], fieldMatched x ] of
            [i] -> return i
            []  -> err "Invalid ADT field identifier"
            _   -> err "Ambiguous ADT field identifier"

        enumMatched <- valsInfix S.EqEq (valI64 idx) =<< adtEnum val

        ptr <- valLocal $ ADT [xs !! idx]
        adtSetPi8 ptr =<< adtPi8 val
        drf <- adtDeref ptr

        case pats of
            []    -> err "Invalid pattern with no arguments."
            [pat] -> valsInfix S.AndAnd enumMatched =<< cmpPattern pat drf
            pats  -> do
                Tuple txs <- assertBaseType isTuple (valType drf)
                assert (length txs == length pats) "Invalid pattern"
                tupVals <- forM (zip txs [0..]) $ \(_, i) -> tupleIdx i drf
                bVals <- zipWithM cmpPattern pats tupVals
                foldM (valsInfix S.AndAnd) (valBool True) (enumMatched:bVals)
    
    _ -> err ("Cannot compile pattern: " ++ show pattern)

