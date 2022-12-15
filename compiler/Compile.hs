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
import LLVM.IRBuilder.Instruction as LL
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Context

import qualified AST
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
import Typeof
import Trace
import Interop
import ADT
import Array
import Builtin
import Symbol
import Sparse
import States



fnHdrToOp :: ModCmp CompileState m => [Type] -> Symbol -> [Type] -> Type -> m LL.Operand
fnHdrToOp paramTypes symbol argTypes returnType = do
    paramOpTypes <- map LL.ptr <$> mapM opTypeOf paramTypes
    argOpTypes   <- mapM opTypeOf argTypes
    returnOpType <- opTypeOf returnType
    let name = fnSymbolToName symbol
    return $ fnOp name (paramOpTypes ++ argOpTypes) returnOpType False



compile :: BoM s m => IRGenState -> m ([LL.Definition], CompileState)
compile irGenState = do
    ((_, defs), state) <- runBoMTExcept (initCompileState $ irModuleName irGenState) (runModuleCmpT emptyModuleBuilder cmp)
    return (defs, state)
    where
        cmp :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        cmp = do
            mainOp <- func (LL.mkName "main")  [] LL.VoidType $ \_ -> do
                cmpTypeDefs irGenState
                cmpTypeNames irGenState
                cmpDeclareExterns irGenState
                cmpFuncHdrs irGenState
                cmpFuncBodies irGenState

            void $ func (LL.mkName $ (irModuleName irGenState) ++ "..callMain")  [] LL.VoidType $ \_ -> do
                void $ call mainOp []



cmpTypeNames :: InsCmp CompileState m => IRGenState -> m ()
cmpTypeNames irGenState = withErrorPrefix "cmpTypeNames" $ do
    forM_ (Map.toList $ irTypeDefs irGenState) $ \(symbol, anno) -> do
        typ <- return $ case anno of
            AST.AnnoType typ -> typ
            AST.AnnoTuple xs -> Tuple (map snd xs)
            AST.AnnoADT xs   -> Void
            AST.AnnoEnum ss  -> Void
            _ -> error (show anno)

        -- when the underlying type is a struct, replace with a type name
        when (isTuple typ || isTable typ || isSparse typ || isRange typ) $ do
            let name = mkNameFromSymbol symbol
            opType <- opTypeOf typ
            typedef name (Just opType)
            modify $ \s -> s { typeNameMap = Map.insert (Typedef symbol) name (typeNameMap s) }


cmpDeclareExterns :: InsCmp CompileState m => IRGenState -> m ()
cmpDeclareExterns irGenState = withErrorPrefix "cmpDeclareExterns: " $ do
    forM_ (Map.toList $ irExternDefs irGenState) $
        \(symbol, (paramTypes, sym, argTypes, returnType)) -> withErrorPrefix (sym ++ " ") $ do
            (flip catchError) (\e -> return ()) $ do
                let name = fnSymbolToName symbol
                paramOpTypes <- map LL.ptr <$> mapM opTypeOf paramTypes
                argOpTypes   <- mapM opTypeOf argTypes
                returnOpType <- opTypeOf returnType
                extern name (paramOpTypes ++ argOpTypes) returnOpType
                define symbol KeyFunc ObjFn


cmpFuncHdrs :: InsCmp CompileState m => IRGenState -> m ()
cmpFuncHdrs irGenState = do
    forM_ (Map.toList $ irFuncDefs irGenState) $ \(symbol, funcBody) -> do
        forM_ (funcArgs funcBody) $ \(AST.Param pos name typ) -> withPos pos $ do
            isDataType <- isDataType typ
            assert (not isDataType) "Cannot use a data type for an argument"

        let paramTypes = map AST.paramType (funcParams funcBody)
        let argTypes   = map AST.paramType (funcArgs funcBody)
        let retty      = funcRetty funcBody
        paramOpTypes <- map LL.ptr <$> mapM opTypeOf paramTypes
        argOpTypes   <- mapM opTypeOf argTypes
        returnOpType <- opTypeOf retty

        let name = fnSymbolToName symbol
        define symbol KeyFunc ObjFn


cmpFuncBodies :: (MonadFail m, Monad m, MonadIO m) => IRGenState -> InstrCmpT CompileState m ()
cmpFuncBodies irGenState = do
    case irMainDef irGenState of
        Nothing   -> return ()
        Just body -> mapM_ cmpStmt (funcStmts $ body)

    forM_ (Map.toList $ irFuncDefs irGenState) $ \(symbol, funcBody) -> do
        let argTypes     = map AST.paramType (funcArgs funcBody)
        let argSymbols   = map AST.paramName (funcArgs funcBody)
        let argNames     = map (ParameterName . mkBSS . Symbol.sym) argSymbols
        let paramTypes   = map AST.paramType (funcParams funcBody)
        let paramSymbols = map AST.paramName (funcParams funcBody)
        let paramNames   = map (ParameterName . mkBSS . Symbol.sym) paramSymbols
        let retty        = funcRetty funcBody

        returnOpType <- opTypeOf retty
        argOpTypes   <- mapM opTypeOf argTypes
        paramOpTypes <- map LL.ptr <$> mapM opTypeOf paramTypes

        ObjFn <- look symbol KeyFunc
        op <- fnHdrToOp paramTypes symbol argTypes retty
        let LL.ConstantOperand (C.GlobalReference _ name) = op

        void $ InstrCmpT . IRBuilderT . lift $ func name (zip (paramOpTypes ++ argOpTypes)  (paramNames ++ argNames)) returnOpType $ \argOps -> do
            forM_ (zip3 paramTypes paramSymbols argOps) $ \(typ, symbol, op) -> do
                define symbol KeyVar (ObjVal $ Ptr typ $ op)

            forM_ (zip3 argTypes (drop (length paramSymbols) argOps) argSymbols) $ \(typ, op, symbol) -> do
                loc <- mkAlloca typ
                valStore loc (Val typ op)
                define symbol KeyVar (ObjVal loc)

            mapM_ cmpStmt (funcStmts funcBody)
            hasTerm <- hasTerminator
            if hasTerm then return ()
            else if retty == Void then retVoid
            else trapMsg "reached end of function" >> unreachable


cmpTypeDefs :: InsCmp CompileState m => IRGenState -> m ()
cmpTypeDefs irGenState = do
    forM_ (Map.toList $ irTypeDefs irGenState) $ \(symbol, anno) ->
        case anno of
            AST.AnnoADT xs   -> adtTypeDef symbol (AST.AnnoADT xs)
            AST.AnnoTuple xs -> do
                define symbol KeyFunc ObjConstructor
                define symbol KeyType (ObjType $ Tuple $ map snd xs)
                forM_ (zip xs [0..]) $ \((s, t), i) -> do
                    define (Sym s) (KeyField $ Typedef symbol) (ObjField i)
            AST.AnnoType typ -> do
                define symbol KeyFunc ObjConstructor
                define symbol KeyType (ObjType typ)
            AST.AnnoEnum ss -> do
                define symbol KeyFunc ObjConstructor
                define symbol KeyType (ObjType Enum)
                forM_ (zip ss [0..]) $ \(s, i) -> do
                    define s (KeyField $ Typedef symbol) (ObjField i)
                    define s KeyFunc (ObjField i)
                    

                    
cmpPrint :: InsCmp CompileState m => AST.Stmt -> m ()
cmpPrint (AST.Print pos exprs) = trace "cmpPrint" $ do
    prints =<< mapM cmpExpr exprs
    where
        prints :: InsCmp CompileState m => [Value] -> m ()
        prints []     = void $ printf "\n" []
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs


cmpStmt :: InsCmp CompileState m => AST.Stmt -> m ()
cmpStmt stmt = trace "cmpStmt" $ withPos stmt $ case stmt of
    AST.Print pos exprs   -> do
        label "print"
        cmpPrint stmt

    AST.Block stmts       -> mapM_ cmpStmt stmts
    AST.ExprStmt expr     -> void $ cmpExpr expr

    AST.Data pos symbol typ mexpr -> do
        loc <- mkAlloca typ
        init <- case mexpr of
            Nothing -> mkZero typ
            Just expr -> cmpExpr expr
        valStore loc init
        define symbol KeyVar (ObjVal loc)

    AST.Assign pos pat expr -> withErrorPrefix "assign: " $ do
        val <- cmpExpr expr
        isDataType <- isDataType (valType val)
        assert (not isDataType) "Cannot use a data type for a variable"
        matched <- valLoad =<< cmpPattern pat val
        let trap = trapMsg ("pattern match failure at: " ++ show (textPos expr))
        if_ (valOp matched) (return ()) trap

    AST.Set pos expr1 expr2 -> do
        label "set"
        loc@(Ptr _ _) <- cmpExpr expr1
        storeCopy loc =<< cmpExpr expr2

    AST.Return pos Nothing -> do
        label "return"
        retVoid
        emitBlockStart =<< fresh

    AST.Return pos (Just expr) -> do
        label "return_expr"
        ret . valOp =<< valLoad =<< cmpExpr expr
        emitBlockStart =<< fresh

    AST.If pos expr blk melse -> do
        label "if"
        val <- valLoad =<< cmpExpr expr
        assertBaseType (== Bool) (valType val)
        if_ (valOp val) (cmpStmt blk) $ maybe (return ()) cmpStmt melse

    AST.While pos cnd blk -> do
        label "while"
        cond <- freshName "while_cond"
        body <- freshName "while_body"
        exit <- freshName "while_exit"

        br cond
        emitBlockStart cond
        val <- valLoad =<< cmpExpr cnd
        assertBaseType (== Bool) (valType val)
        condBr (valOp val) body exit
        
        emitBlockStart body
        cmpStmt blk
        br cond
        emitBlockStart exit

    AST.Switch _ expr cases -> do
        label "switch"
        val <- cmpExpr expr
        let cases' = [(fmap valOp (cmpPattern pat val), cmpStmt stmt) | (pat, stmt) <- cases]
        let trap   = trapMsg ("switch match failure at: " ++ show (textPos stmt))
        switch_ $ cases' ++ [(return (bit 1), trap)]

    AST.For _ expr mpat blk -> do
        label "for"
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)

        idx <- mkAlloca I64
        valStore idx =<< mkZero I64
        when (isRange base) $ do
            valStore idx =<< mkRangeStart val

        cond <- freshName "for_cond"
        body <- freshName "for_body"
        patm <- freshName "for_patm"
        incr <- freshName "for_incr"
        exit <- freshName "for_exit"

        br cond

        -- check if index is still in range
        emitBlockStart cond
        end <- case base of
            Range I64 -> mkRangeEnd val
            String -> mkStringLen I64 val
            Table ts -> mkTableLen val
            Array n t -> return (mkI64 n)
            _ -> error (show base)
        ilt <- mkIntInfix AST.LT idx end
        condBr (valOp ilt) patm exit

        -- match pattern (if present)
        emitBlockStart patm
        patMatch <- case mpat of
            Nothing -> mkBool Bool True
            Just pat -> case base of
                String -> cmpPattern pat =<< mkStringIdx val idx
                Table ts -> do
                    [v] <- ptrsTableColumn val idx
                    cmpPattern pat v
                Range I64 -> cmpPattern pat idx
                Array n t -> cmpPattern pat =<< ptrArrayGetElem val idx
                _ -> error (show base)
        condBr (valOp patMatch) body exit
        
        -- for loop body
        emitBlockStart body
        cmpStmt blk
        br incr

        -- increment index
        emitBlockStart incr
        valStore idx =<< mkIntInfix AST.Plus idx (mkI64 1)
        br cond

        emitBlockStart exit

    _ -> error (show stmt)
    where
        label :: InsCmp CompileState m => String -> m ()
        label str = do
            let pos = textPos stmt
            name <- freshName $ mkBSS $ str ++ " " ++ show (textLine pos)
            --printf (str ++ " " ++ show (Error.textLine pos) ++ "\n") []
            br name
            emitBlockStart name

-- must return Val unless local variable
cmpExpr :: InsCmp CompileState m =>  AST.Expr -> m Value
cmpExpr (AST.AExpr exprType expr) = withErrorPrefix "expr: " $ withPos expr $ withCheck exprType $ case expr of
    AST.Bool pos b               -> mkBool exprType b
    AST.Char pos c               -> mkChar exprType c
    AST.Tuple pos [expr]         -> cmpExpr expr
    AST.Float p f                -> mkFloat exprType f
    AST.Prefix pos operator expr -> mkPrefix operator =<< cmpExpr expr
    AST.Field pos expr sym       -> valTupleField sym =<< cmpExpr expr
    AST.Null p                   -> adtNull exprType
    AST.Match pos expr pat       -> cmpPattern pat =<< cmpExpr expr

    AST.Conv pos typ [expr]      -> do
        val <- mkConvert typ =<< cmpExpr expr
        base <- baseTypeOf exprType
        case base of
            _ | exprType == valType val -> return val -- char(3):char
            ADT xs | FieldType typ `elem` xs -> do    -- char(3):{char | null}
                loc <- mkMalloc typ (mkI64 1)
                storeCopy loc val
                adtConstructFromPtr exprType loc
            _ -> fail $ "cannot convert to type: " ++ show exprType
                

            _ -> error $ show (base, typ)


    AST.Range pos Nothing _ Nothing -> fail "Range expression must contain maximum"
    AST.Range pos Nothing mexpr1 (Just expr2) -> do
        end <- cmpExpr expr2
        start <- maybe (mkZero $ valType end) cmpExpr mexpr1
        mkRange start end
    AST.Range pos (Just expr) margStart margEnd -> do
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)

        -- default 0 because all types index 0
        startVal <- case base of
            Range t -> mkRangeStart val
            _       -> return $ mkI64 0
        start <- case margStart of
            Nothing  -> return $ startVal
            Just arg -> mkMax startVal =<< cmpExpr arg

        endVal <- case base of
            Range t   -> mkRangeEnd val
            Array n t -> return $ mkI64 n
            String    -> mkStringLen I64 val
            Table ts  -> mkTableLen val
        end <- case margEnd of
            Nothing  -> return endVal
            Just arg -> mkMin endVal =<< cmpExpr arg
            
        mkRange start end

    AST.Push pos expr [] -> do
        loc <- cmpExpr expr
        base <- baseTypeOf (valType loc)
        case base of
            Table ts -> do
                len <- mkTableLen loc
                tableResize loc =<< mkIntInfix AST.Plus len (mkI64 1)
                ptrs <- ptrsTableColumn loc len
                forM_ ptrs $ \ptr -> valStore ptr =<< mkZero (valType ptr)
                mkConvertNumber exprType =<< valLoad len
            Sparse ts -> do
                key <- sparsePush loc =<< mapM mkZero ts
                valLoad key

    AST.Push pos expr exprs -> do
        tab <- cmpExpr expr
        Table ts <- assertBaseType isTable (valType tab)
        vals <- mapM cmpExpr exprs
        assert (map valType vals == ts) "mismatched argument types"

        len <- mkTableLen tab
        tableResize tab =<< mkIntInfix AST.Plus len (mkI64 1)
        ptrs <- ptrsTableColumn tab len
        zipWithM_ storeCopy ptrs vals
        valLoad len

    AST.Pop pos expr [] -> do
        val <- cmpExpr expr
        [v] <- valTablePop val
        loc <- mkAlloca exprType
        storeCopy loc v
        return loc

    AST.Clear pos expr -> do
        assert (exprType == Void) "clear is a void expression"
        tab <- cmpExpr expr
        tableResize tab (mkI64 0)
        return $ Val Void undefined

    AST.Delete pos expr1 expr2 -> do
        assert (exprType == Void) "delete returns void"
        val <- cmpExpr expr1
        arg <- cmpExpr expr2
        base <- baseTypeOf (valType val)
        case base of
            Sparse ts -> sparseDelete val arg
        return $ Val Void undefined
                
    AST.Int p n -> do
        base <- baseTypeOf exprType
        case base of
            _ | isInt base   -> mkInt exprType n
            _ | isFloat base -> mkFloat exprType (fromIntegral n)
            _ | base == Char -> mkChar exprType (chr $ fromIntegral n)
            _ | otherwise    -> fail $ "invalid base type of: " ++ show base

    AST.Infix pos AST.AndAnd exprA exprB -> do
        assertBaseType (== Bool) exprType
        exit <- freshName "infix_andand_exit"
        right <- freshName "infix_andand_rhs"

        b <- mkAlloca exprType
        valStore b =<< mkBool exprType False

        valA <- cmpExpr exprA
        assertBaseType (== Bool) (valType valA)
        aTrue <- valOp <$> valLoad valA
        condBr aTrue right exit

        emitBlockStart right
        valStore b =<< cmpExpr exprB
        br exit

        emitBlockStart exit
        valLoad b
        
    AST.Infix pos op exprA exprB -> do
        valA <- cmpExpr exprA
        valB <- cmpExpr exprB
        mkInfix op valA valB

    AST.Ident pos symbol -> do
        obj <- look symbol KeyVar
        case obj of
            ObjVal (ConstInt n) -> return (mkI64 n)
            ObjVal loc -> return loc
            
    AST.String pos s -> do
        base <- baseTypeOf exprType
        loc <- getStringPointer s
        case base of
            String -> do
                return (Val exprType loc)
            Table [Char] -> do
                tab <- mkAlloca exprType
                tableSetCap tab $ mkI64 (length s)
                tableSetLen tab $ mkI64 (length s)
                tableSetRow tab 0 $ Ptr Char loc
                return tab
                

    AST.Call pos params symbol args  -> do
        ps <- mapM cmpExpr params
        as <- mapM cmpExpr args

        -- TODO this calls alloca on values...
        psLocs <- forM ps $ \val -> case val of
            Val _ op -> do 
                local <- mkAlloca (valType val)
                valStore local val
                return $ valLoc local
            Ptr _ loc -> return loc

        asOps <- map valOp <$> mapM valLoad as
        obj <- look symbol KeyFunc
        case obj of
            ObjFn -> do
                op <- fnHdrToOp (map valType ps) symbol (map valType as) exprType
                Val exprType <$> call op [(o, []) | o <- psLocs ++ asOps]
            ObjConstructor  -> mkConstruct exprType as
            ObjField i      -> do
                base <- baseTypeOf exprType
                case base of
                    ADT _ -> adtConstructField symbol exprType as
                    Enum  -> mkEnum exprType i

    
    AST.Len pos expr -> valLoad =<< do
        assertBaseType isIntegral exprType
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)
        case base of
            Table _   -> mkConvertNumber exprType =<< mkTableLen val
            Array n t -> mkConvertNumber exprType (mkI64 n)
            String    -> mkStringLen exprType val
            _       -> fail ("cannot take length of type " ++ show (valType val))

    AST.Tuple pos exprs -> do
        Tuple ts <- assertBaseType isTuple exprType
        vals <- mapM cmpExpr exprs
        assert (ts == map valType vals) $
            "Incorrect val types: " ++ show ts ++ ", " ++ show (map valType vals)

        tup <- mkAlloca exprType
        forM_ (zip vals [0..]) $ \(v, i) -> do
            ptr <- ptrTupleIdx i tup
            valStore ptr v
        return tup

    AST.TupleIndex pos expr n -> do
        val <- cmpExpr expr
        ptrTupleIdx (fromIntegral n) val

    AST.Subscript pos expr idxExpr -> withErrorPrefix "subscript: " $ do
        val <- cmpExpr expr
        idx <- cmpExpr idxExpr
        base <- baseTypeOf (valType val)
        case base of
            Table _   -> do
                [v] <- ptrsTableColumn val idx
                return v
            Array _ _ -> ptrArrayGetElem val idx
            String    -> mkStringIdx val idx 
            Sparse _  -> do
                table <- ptrSparseTable val
                [v] <- ptrsTableColumn table idx
                return v
            Range t -> do
                start <- mkRangeStart val
                end <- mkRangeEnd val
                idxGtEqStart <- mkInfix AST.GTEq idx start
                idxLtEnd <- mkInfix AST.LT idx end
                assertBaseType (== Bool) exprType
                Val exprType <$> LL.and (valOp idxLtEnd) (valOp idxGtEqStart)
                
    AST.Initialiser pos exprs -> do
        base <- baseTypeOf exprType
        case base of
            Array n t -> do
                vals <- mapM cmpExpr exprs
                assert (length vals == n) "Invalid array length"
                arr <- mkAlloca exprType
                forM_ (zip vals [0..]) $ \(val, i) -> do
                    ptr <- ptrArrayGetElemConst arr i
                    valStore ptr val
                return arr

            _ -> fail $ "invalid initialiser base type: " ++ show base

    AST.UnsafePtr pos expr -> do
        val <- cmpExpr expr
        let UnsafePtr t = exprType
        assertBaseType (== t) (valType val)
        case t of
            Char -> case val of
                Ptr _ loc -> return $ Val (UnsafePtr t) loc
                Val _ _   -> fail $ "cannot take pointer of value"
            _ -> fail (show t)

    AST.ADT pos expr -> do
        val <- cmpExpr expr
        mal <- mkMalloc (valType val) (mkI64 1)
        storeCopy mal val
        adtConstructFromPtr exprType mal

    _ -> fail ("invalid expression: " ++ show expr)
    where
        withCheck :: InsCmp CompileState m => Type -> m Value -> m Value
        withCheck typ m = do
            val <- m
            isDataType <- isDataType (valType val)
            assert (valType val == typ) $ "Expression compiled to: " ++ show (valType val) ++ " instead of: " ++ show typ
            return val
            

cmpPattern :: InsCmp CompileState m => AST.Pattern -> Value -> m Value
cmpPattern pattern val = withErrorPrefix "pattern: " $ withPos pattern $ case pattern of
    AST.PatIgnore _     -> mkBool Bool True
    AST.PatLiteral expr -> mkInfix AST.EqEq val =<< cmpExpr expr

    AST.PatNull _ -> do -- null
        base@(ADT fs) <- assertBaseType isADT (valType val)
        let is = elemIndices FieldNull fs
        assert (length is == 1) "ADT type does not have unique null field"
        let [i] = is
        mkIntInfix AST.EqEq (mkI64 i) =<< adtEnum val

    AST.PatAnnotated pat typ -> do -- a:i32
        assert (valType val == typ) "pattern type mismatch"
        cmpPattern pat val

    AST.PatGuarded _ pat expr -> do -- a | a > 0
        match <- cmpPattern pat =<< valLoad val
        guard <- cmpExpr expr
        assertBaseType (== Bool) (valType guard)
        mkInfix AST.AndAnd match guard

    AST.PatIdent _ symbol -> trace ("cmpPattern " ++ show pattern) $ do -- a
        base <- baseTypeOf (valType val)
        loc <- mkAlloca (valType val)
        valStore loc val
        define symbol KeyVar (ObjVal loc)
        mkBool Bool True

    AST.PatTuple _ pats -> do -- (a, b)
        len <- tupleLength val
        assert (len == length pats) "incorrect tuple length"

        bs <- forM (zip pats [0..]) $ \(p, i) ->
            cmpPattern p =<< valTupleIdx i val

        true <- mkBool Bool True
        foldM (mkInfix AST.AndAnd) true bs

    AST.PatArray _ pats -> do -- [a, b, c]
        base <- baseTypeOf (valType val)
        case base of
            Table ts -> do
                len   <- mkTableLen val
                lenEq <- mkInfix AST.EqEq len (mkI64 $ length pats)

                assert (length ts == 1) "patterns don't support multiple rows (yet)"
                bs <- forM (zip pats [0..]) $ \(p, i) -> do
                    [v] <- ptrsTableColumn val (mkI64 i)
                    cmpPattern p v

                true <- mkBool (valType lenEq) True
                foldM (mkInfix AST.AndAnd) true (lenEq:bs)

            Array n t -> do
                assert (n == length pats) "Invalid array pattern"
                bs <- forM (zip pats [0..]) $ \(p, i) ->
                    cmpPattern p =<< ptrArrayGetElemConst val i

                true <- mkBool Bool True
                foldM (mkInfix AST.AndAnd) true bs
            
            _ -> fail "Invalid array pattern"

    AST.PatField _ symbol pats -> do -- symbol(a, b, c)
        base <- baseTypeOf (valType val) 
        case base of
            Enum -> do
                assert (pats == []) "enum pattern with args"
                ObjField i <- look symbol (KeyField $ valType val)
                mkInfix AST.EqEq val =<< mkEnum (valType val) i

            ADT fs -> do
                obj <- look symbol (KeyField $ valType val)
                case obj of
                    ObjAdtTypeField i -> do
                        assert (length pats == 1)       "One pattern allowed for type field"
                        enumMatch <- mkIntInfix AST.EqEq (mkI64 i) =<< adtEnum val
                        Ptr _ loc <- adtDeref val i 0
                        ObjType t0 <- look symbol KeyType
                        b <- cmpPattern (head pats) $ Ptr t0 loc
                        valLoad =<< mkInfix AST.AndAnd enumMatch b

                    ObjField i -> do
                        let FieldCtor ts = fs !! i
                        assert (length pats == length ts) "invalid ADT pattern"
                        enumMatch <- mkIntInfix AST.EqEq (mkI64 i) =<< adtEnum val
                        -- can't be inside a block which may or may not happen
                        -- as cmpPattern may add variables to the symbol table
                        bs <- forM (zip pats [0..]) $ \(pat, j) -> 
                            cmpPattern pat =<< adtDeref val i j

                        valLoad =<< foldM (mkInfix AST.AndAnd) enumMatch bs


    AST.PatTypeField _ typ pat -> do -- char(c)
        ADT fs <- assertBaseType isADT (valType val)
        i <- case valType val of
            Typedef symbol -> do
                ObjField i <- look symbol (KeyTypeField typ)
                return i
            _ -> do
                let is = elemIndices (FieldType typ) fs
                assert (length is == 1) "ADT does not contain unique type field"
                return (head is)

        match <- freshName "adt_pat_match"
        exit  <- freshName "adt_pat_exit"

        matched <- mkAlloca Bool
        valStore matched =<< mkBool Bool False
        enumMatch <- mkIntInfix AST.EqEq (mkI64 i) =<< adtEnum val
        condBr (valOp enumMatch) match exit

        emitBlockStart match
        valStore matched =<< cmpPattern pat =<< adtDeref val i 0
        br exit

        emitBlockStart exit
        valLoad matched




