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
import Typeof
import Trace
import Interop
import ADT
import Array
import Builtin
import Symbol
import Sparse


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
            typedef (mkName "String") $
                Just (LL.StructureType False [LL.i64, LL.i64, LL.ptr LL.i8])
            
            forM_ imports $ \imp -> do
                forM_ (Map.elems $ typeMap imp) $ \(name, opType) -> do
                    typedef name (Just opType)


            mainOp <- func (LL.mkName "main")  [] LL.VoidType $ \_ -> do
                mapM_ cmpTypeDef   [ stmt | stmt@(S.Typedef _ _ _) <- S.astStmts ast ]
                mapM_ cmpFuncHdr   [ stmt | stmt@(S.FuncDef _ _ _ _ _ _) <- S.astStmts ast ]
                mapM_ cmpDataDef   [ stmt | stmt@(S.Data _ _ _) <- S.astStmts ast ]
                mapM_ cmpVarDef    [ stmt | stmt@(S.Assign _ _ _) <- S.astStmts ast ]
                mapM_ cmpFuncDef   [ stmt | stmt@(S.FuncDef _ _ _ _ _ _) <- S.astStmts ast ]

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
        t | isTable t -> tableTypeDef symbol (S.AnnoType t)
        t             -> do
            let typdef = Typedef symbol
            define symbol (KeyFunc [] typdef) ObjConstructor
            define symbol (KeyFunc [t] typdef) ObjConstructor
            define symbol (KeyFunc [typdef] typdef) ObjConstructor
            define symbol KeyType (ObType t Nothing)
                    

cmpDataDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpDataDef (S.Data pos symbol typ) = withPos pos $ do
    name <- myFresh (sym symbol)
    initialiser <- mkZero typ
    opTyp <- opTypeOf typ
    loc <- Ptr typ <$> global name opTyp (toCons $ valOp initialiser)
    define symbol KeyVar (ObjVal loc)
    addSymKeyDec symbol KeyVar name (DecVar opTyp)
    addDeclared name


cmpVarDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpVarDef (S.Assign pos (S.PatIdent p symbol) expr) = withPos pos $ do
    val <- cmpExpr expr
    name <- myFresh (sym symbol)

    let typ = valType val
    isDataType <- isDataType typ
    assert (not isDataType) "Cannot use a data type for a variable"


    opTyp <- opTypeOf typ

    if isCons (valOp val)
    then do
        loc <- Ptr typ <$> global name opTyp (toCons $ valOp val)
        define symbol KeyVar (ObjVal loc)
    else do
        initialiser <- mkZero typ
        loc <- Ptr typ <$> global name opTyp (toCons $ valOp initialiser)
        valStore loc val
        define symbol KeyVar (ObjVal loc)

    addSymKeyDec symbol KeyVar name (DecVar opTyp)
    addDeclared name


cmpFuncHdr :: InsCmp CompileState m => S.Stmt -> m ()
cmpFuncHdr (S.FuncDef pos Nothing "main" [] Void blk) = return ()
cmpFuncHdr (S.FuncDef pos _ "main" _ _ _)             = withPos pos $ fail "invalid main func"
cmpFuncHdr (S.FuncDef pos mparam sym args retty blk)  = trace "cmpFuncHdr" $ withPos pos $ do

    forM_ args $ \(S.Param pos name typ) -> withPos pos $ do
        isDataType <- isDataType typ
        assert (not isDataType) "Cannot use a data type for an argument"
    isDataType <- isDataType retty
    assert (not isDataType) "Cannot return a data type"

    name <- myFresh sym
    let argTypes = map S.paramType args
    argOpTypes <- mapM opTypeOf argTypes
    returnOpType <- opTypeOf retty

    case mparam of
        Nothing -> do
            let op = fnOp name argOpTypes returnOpType False
            define (Sym sym) (KeyFunc argTypes retty) (ObjFnOp op) 
            --redefine (Sym sym) KeyVar $ ObjVal $ Val (Func argTypes retty) op
            addSymKeyDec (Sym sym) (KeyFunc argTypes retty) name (DecFunc argOpTypes returnOpType)
            addSymKeyDec (Sym sym) KeyVar name (DecFunc argOpTypes returnOpType)
            addDeclared name

        Just param -> do
            let paramType = S.paramType param
            paramOpTyp <- LL.ptr <$> opTypeOf paramType
            let op = fnOp name (paramOpTyp : argOpTypes) returnOpType False
            define (Sym sym) (KeyMember paramType argTypes retty) (ObjFnOp op) 
            addSymKeyDec (Sym sym) (KeyMember paramType argTypes retty) name (DecFunc (paramOpTyp : argOpTypes) returnOpType)
            addDeclared name


cmpFuncDef :: (MonadFail m, Monad m, MonadIO m) => S.Stmt -> InstrCmpT CompileState m ()
cmpFuncDef (S.FuncDef pos mparam "main" args retty blk) = trace "cmpFuncDef" $ withPos pos $ do
    assert (isNothing mparam) "main cannot be a member"
    assert (args == [])  "main cannot have argeters"
    assert (retty == Void) $ "main must return void: " ++ show retty
    cmpStmt blk
cmpFuncDef (S.FuncDef pos mparam sym args retty blk) = trace "cmpFuncDef" $ withPos pos $ do
    returnOpType <- opTypeOf retty
    argOpTypes <- mapM (opTypeOf . S.paramType) args
    let argTypes = map S.paramType args
    let argSymbols = map S.paramName args
    let argNames = map (ParameterName . mkBSS . Symbol.sym) argSymbols

    case mparam of
        Just param -> do
            let paramType = S.paramType param
            let paramSymbol = S.paramName param
            paramOpTyp <- LL.ptr <$> opTypeOf paramType
            let paramName = ParameterName $ mkBSS $ Symbol.sym $ paramSymbol
            ObjFnOp op <- look (Sym sym) (KeyMember paramType argTypes retty)

            let LL.ConstantOperand (C.GlobalReference _ name) = op
            let Name nameStr = name
            void $ InstrCmpT . IRBuilderT . lift $ func name (zip (paramOpTyp : argOpTypes)  (paramName :argNames)) returnOpType $ \argOps -> do
                define paramSymbol KeyVar (ObjVal $ Ptr paramType $ head argOps)
                forM_ (zip3 argTypes (tail argOps) argSymbols) $ \(typ, op, symbol) -> do
                    loc <- mkAlloca typ
                    valStore loc (Val typ op)
                    define symbol KeyVar (ObjVal loc)
                cmpFnBlock
        Nothing -> do
            ObjFnOp op <- look (Sym sym) (KeyFunc argTypes retty)

            let LL.ConstantOperand (C.GlobalReference _ name) = op
            let Name nameStr = name
            void $ InstrCmpT . IRBuilderT . lift $ func name (zip argOpTypes argNames) returnOpType $ \argOps -> do
                forM_ (zip3 argTypes argOps argSymbols) $ \(typ, op, symbol) -> do
                    loc <- mkAlloca typ
                    valStore loc (Val typ op)
                    define symbol KeyVar (ObjVal loc)
                cmpFnBlock
    where
        cmpFnBlock :: InsCmp CompileState m => m ()
        cmpFnBlock = do
            cmpStmt blk
            hasTerm <- hasTerminator
            if hasTerm
            then return ()
            else if retty == Void
            then retVoid
            else unreachable



cmpPrint :: InsCmp CompileState m => S.Stmt -> m ()
cmpPrint (S.Print pos exprs) = trace "cmpPrint" $ do
    prints =<< mapM cmpExpr exprs
    where
        prints :: InsCmp CompileState m => [Value] -> m ()
        prints []     = void $ printf "\n" []
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs


cmpStmt :: InsCmp CompileState m => S.Stmt -> m ()
cmpStmt stmt = trace "cmpStmt" $ withPos stmt $ case stmt of
    S.Print pos exprs   -> do
        label "print"
        cmpPrint stmt

    S.Block stmts       -> mapM_ cmpStmt stmts

    S.ExprStmt expr     -> void $ cmpExpr expr

    S.Data pos symbol typ -> do
        loc <- mkAlloca typ
        valStore loc =<< mkZero (valType loc)
        define symbol KeyVar (ObjVal loc)

    S.Assign pos pat expr -> withErrorPrefix "assign: " $ do
        val <- cmpExpr expr
        isDataType <- isDataType (valType val)
        assert (not isDataType) "Cannot use a data type for a variable"
        matched <- valLoad =<< cmpPattern pat val
        if_ (valOp matched) (return ()) (void trap) 

    S.Set pos expr1 expr2 -> do
        label "set"
        loc@(Ptr _ _) <- cmpExpr expr1
        base <- baseTypeOf (valType loc)
        val <- cmpExpr expr2
        case base of
            _ | isSimple base -> valStore loc val
            Table ts -> do 
                tableClear loc
                tableAppend loc val
            Array n t -> valStore loc val
            _ | isEnumADT base -> valStore loc val
            _ -> error (show base)

    S.Return pos Nothing -> do
        label "return"
        retVoid
        emitBlockStart =<< fresh

    S.Return pos (Just expr) -> do
        label "return_expr"
        ret . valOp =<< valLoad =<< cmpExpr expr
        emitBlockStart =<< fresh

    S.If pos expr blk melse -> do
        label "if"
        val <- valLoad =<< cmpExpr expr
        assertBaseType (== Bool) (valType val)
        if_ (valOp val) (cmpStmt blk) $ maybe (return ()) cmpStmt melse

    S.While pos cnd blk -> do
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

    S.Typedef _ _ _ -> cmpTypeDef stmt
    
    S.Switch _ expr cases -> do
        label "switch"
        val <- cmpExpr expr
        let cases' = [(fmap valOp (cmpPattern pat val), cmpStmt stmt) | (pat, stmt) <- cases]
        switch_ $ cases' ++ [(return (bit 1), void trap)]

    S.For _ expr mpat blk -> do
        label "for"
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)

        idx <- mkAlloca I64
        valStore idx =<< mkZero I64
        when (isRange base) $ do
            valStore idx =<< ptrRangeStart val

        cond <- freshName "for_cond"
        body <- freshName "for_body"
        patm <- freshName "for_patm"
        incr <- freshName "for_incr"
        exit <- freshName "for_exit"

        br cond

        -- check if index is still in range
        emitBlockStart cond
        end <- case base of
            Range I64 -> ptrRangeEnd val
            String -> mkStringLen I64 val
            Table ts -> tableLen val
            Array n t -> return (mkI64 n)
            _ -> error (show base)
        ilt <- mkIntInfix S.LT idx end
        condBr (valOp ilt) patm exit

        -- match pattern (if present)
        emitBlockStart patm
        patMatch <- case mpat of
            Nothing -> mkBool Bool True
            Just pat -> case base of
                String -> cmpPattern pat =<< mkStringIdx val idx
                Table ts -> do
                    [v] <- tableGetColumn val idx
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
        valStore idx =<< mkIntInfix S.Plus idx (mkI64 1)
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
cmpExpr :: InsCmp CompileState m =>  S.Expr -> m Value
cmpExpr (S.AExpr exprType expr) = trace "cmpExpr" $ withPos expr $ withCheck exprType $ case expr of
    S.Bool pos b               -> mkBool exprType b
    S.Char pos c               -> mkChar exprType c
    S.Conv pos typ [expr]      -> mkConvert typ =<< cmpExpr expr
    S.Tuple pos [expr]         -> cmpExpr expr
    S.Float p f                -> mkFloat exprType f
    S.Prefix pos operator expr -> mkPrefix operator =<< cmpExpr expr
    S.Field pos expr sym       -> valTupleField sym =<< cmpExpr expr
    S.Null p                   -> adtNull exprType
    S.Match pos expr pat       -> cmpPattern pat =<< cmpExpr expr

    S.Range pos Nothing _ Nothing -> fail "Range expression must contain maximum"
    S.Range pos Nothing mexpr1 (Just expr2) -> do
        start <- maybe (return $ mkI64 0) cmpExpr mexpr1
        end <- cmpExpr expr2
        mkRange start end
    S.Range pos (Just expr) mexpr1 mexpr2 -> do
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)
        start <- maybe (return $ mkI64 0) cmpExpr mexpr1
        end <- case base of
            Array n t -> maybe (return $ mkI64 n) cmpExpr mexpr2
            Table ts  -> tableLen val
            String    -> mkStringLen I64 val
            _         -> error (show base)
        mkRange start end

    S.Push pos expr [] -> do
        loc <- cmpExpr expr
        base <- baseTypeOf (valType loc)
        case base of
            Table ts -> do
                len <- tableLen loc
                tableAppendColumn loc =<< mapM mkZero ts
                mkConvertNumber exprType =<< valLoad len
            Sparse ts -> do
                key <- sparsePush loc =<< mapM mkZero ts
                valLoad key

    S.Push pos expr exprs -> do
        loc <- cmpExpr expr
        len <- tableLen loc
        tableAppendColumn loc =<< mapM cmpExpr exprs
        valLoad len

    S.Pop pos expr [] -> do
        loc@(Ptr _ _) <- cmpExpr expr
        [v] <- tablePop loc
        return v

    S.Clear pos expr -> do
        assert (exprType == Void) "clear is a void expression"
        tab <- cmpExpr expr
        tableClear tab
        return $ Val Void (valLoc tab)

    S.Delete pos expr1 expr2 -> do
        assert (exprType == Void) "delete returns void"
        val <- cmpExpr expr1
        arg <- cmpExpr expr2
        base <- baseTypeOf (valType val)
        case base of
            Sparse ts -> sparseDelete val arg

        return $ Val Void undefined
                
        

    S.Int p n -> do
        base <- baseTypeOf exprType
        case base of
            _ | isInt base   -> mkInt exprType n
            _ | isFloat base -> mkFloat exprType (fromIntegral n)
            _ | base == Char -> mkChar exprType (chr $ fromIntegral n)
            _ | otherwise    -> fail $ "invalid base type of: " ++ show base

    S.Infix pos op exprA exprB -> do
        valA <- cmpExpr exprA
        valB <- cmpExpr exprB
        mkInfix op valA valB

    S.Ident pos symbol -> do
        obj <- look symbol KeyVar
        case obj of
            ObjVal (ConstInt n) -> return (mkI64 n)
            ObjVal loc -> return loc
            
    S.String pos s -> do
        assertBaseType (== String) exprType
        loc <- getStringPointer s
        return (Val exprType loc)

    S.CallMember pos expr symbol exprs  -> do
        val <- cmpExpr expr
        -- TODO possible to cause incorrect logic?
        loc <- case val of
            Val _ op -> do 
                local <- mkAlloca (valType val)
                valStore local val
                return $ valLoc local
            Ptr _ loc -> return loc
        vals <- mapM valLoad =<< mapM cmpExpr exprs
        obj <- look symbol $ KeyMember (valType val) (map valType vals) exprType
        case obj of
            ObjFnOp op -> Val exprType <$> call op [(o, []) | o <- loc : map valOp vals]

    S.Call pos symbol exprs -> do
        vals <- mapM valLoad =<< mapM cmpExpr exprs
        obj <- look symbol $ KeyFunc (map valType vals) exprType
        case obj of
            ObjConstructor  -> mkConstruct exprType vals
            ObjFnOp op      -> Val exprType <$> call op [(o, []) | o <- map valOp vals]
            ObjField i      -> adtConstructField symbol exprType vals
    
    S.Len pos expr -> valLoad =<< do
        assertBaseType isIntegral exprType
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)
        case base of
            Table _   -> mkConvertNumber exprType =<< tableLen val
            Array n t -> mkConvertNumber exprType (mkI64 n)
            String    -> mkStringLen exprType val
            _       -> fail ("cannot take length of type " ++ show (valType val))

    S.Tuple pos exprs -> do
        Tuple ts <- assertBaseType isTuple exprType
        vals <- mapM cmpExpr exprs
        assert (ts == map valType vals) $
            "Incorrect val types: " ++ show ts ++ ", " ++ show (map valType vals)

        tup <- mkAlloca exprType
        forM_ (zip vals [0..]) $ \(v, i) -> do
            ptr <- ptrTupleIdx i tup
            valStore ptr v
        return tup

    S.TupleIndex pos expr n -> do
        val <- cmpExpr expr
        ptrTupleIdx (fromIntegral n) val

    S.Subscript pos expr idxExpr -> withErrorPrefix "subscript: " $ do
        val <- cmpExpr expr
        idx <- cmpExpr idxExpr
        base <- baseTypeOf (valType val)
        case base of
            Table _   -> do
                [v] <- tableGetColumn val idx
                return v
            Array _ _ -> ptrArrayGetElem val idx
            String    -> mkStringIdx val idx 
            Sparse _  -> do
                [v] <- sparseGetColumn val idx
                return v
            Range t -> do
                start <- ptrRangeStart val
                end <- ptrRangeEnd val
                idxGtEqStart <- mkInfix S.GTEq idx start
                idxLtEnd <- mkInfix S.LT idx end
                assertBaseType (== Bool) exprType
                Val exprType <$> LL.and (valOp idxLtEnd) (valOp idxGtEqStart)
                

    S.Array pos exprs -> do
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
                
            _ -> fail $ "invalid array base type: " ++ show base

    S.UnsafePtr pos expr -> do
        val <- cmpExpr expr
        let UnsafePtr t = exprType
        assertBaseType (== t) (valType val)
        case t of
            Char -> case val of
                Ptr _ loc -> return $ Val (UnsafePtr t) loc
                Val _ _   -> fail $ "cannot take pointer of value"
            _ -> fail (show t)

    S.ADT pos expr -> do
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
            


cmpPattern :: InsCmp CompileState m => S.Pattern -> Value -> m Value
cmpPattern pattern val = withErrorPrefix "pattern: " $ withPos pattern $ case pattern of
    S.PatIgnore _     -> mkBool Bool True
    S.PatLiteral expr -> mkInfix S.EqEq val =<< cmpExpr expr

    S.PatNull _ -> do
        base@(ADT fs) <- assertBaseType isADT (valType val)
        let is = elemIndices FieldNull fs
        assert (length is == 1) "ADT type does not have unique null field"
        let [i] = is
        mkIntInfix S.EqEq (mkI64 i) =<< adtEnum val

    S.PatAnnotated pat typ -> do
        assert (valType val == typ) "pattern type mismatch"
        cmpPattern pat val

    S.PatGuarded _ pat expr -> do
        match <- cmpPattern pat =<< valLoad val
        guard <- cmpExpr expr
        assertBaseType (== Bool) (valType guard)
        mkInfix S.AndAnd match guard

    S.PatIdent _ symbol -> trace ("cmpPattern " ++ show pattern) $ do
        base <- baseTypeOf (valType val)
        loc <- mkAlloca (valType val)
        valStore loc val
        define symbol KeyVar (ObjVal loc)
        mkBool Bool True

    S.PatTuple _ pats -> do
        len <- tupleLength val
        assert (len == length pats) "incorrect tuple length"

        bs <- forM (zip pats [0..]) $ \(p, i) ->
            cmpPattern p =<< valTupleIdx i val

        true <- mkBool Bool True
        foldM (mkInfix S.AndAnd) true bs

    S.PatArray _ pats -> do
        base <- baseTypeOf (valType val)
        case base of
            Table ts -> do
                len   <- tableLen val
                lenEq <- mkInfix S.EqEq len (mkI64 $ length pats)

                assert (length ts == 1) "patterns don't support multiple rows (yet)"
                bs <- forM (zip pats [0..]) $ \(p, i) -> do
                    [v] <- tableGetColumn val (mkI64 i)
                    cmpPattern p v

                true <- mkBool (valType lenEq) True
                foldM (mkInfix S.AndAnd) true (lenEq:bs)

            Array n t -> do
                assert (n == length pats) "Invalid array pattern"
                bs <- forM (zip pats [0..]) $ \(p, i) ->
                    cmpPattern p =<< ptrArrayGetElemConst val i

                true <- mkBool Bool True
                foldM (mkInfix S.AndAnd) true bs
            
            _ -> fail "Invalid array pattern"

    S.PatField _ symbol pats -> do
        base@(ADT fs) <- assertBaseType isADT (valType val)
        obj <- look symbol (KeyField $ valType val)
        case obj of
            ObjAdtTypeField i -> do
                assert (length pats == 1)       "One pattern allowed for type field"
                enumMatch <- mkIntInfix S.EqEq (mkI64 i) =<< adtEnum val
                Ptr _ loc <- adtDeref val i 0
                ObType t0 _ <- look symbol KeyType
                b <- cmpPattern (head pats) $ Ptr t0 loc
                valLoad =<< mkInfix S.AndAnd enumMatch b


            ObjField i -> do
                let FieldCtor ts = fs !! i
                assert (length pats == length ts) "invalid ADT pattern"

                case base of
                    _ | isEnumADT base -> do
                        assert (length pats == 0) "invalid ADT pattern"
                        mkIntInfix S.EqEq (mkI64 i) =<< adtEnum val

                    _ | isNormalADT base -> do
                        enumMatch <- mkIntInfix S.EqEq (mkI64 i) =<< adtEnum val
                        -- can't be inside a block which may or may not happen
                        -- as cmpPattern may add variables to the symbol table
                        bs <- forM (zip pats [0..]) $ \(pat, j) -> 
                            cmpPattern pat =<< adtDeref val i j

                        valLoad =<< foldM (mkInfix S.AndAnd) enumMatch bs


    S.PatTypeField _ typ pat -> do
        base@(ADT fs) <- assertBaseType isADT (valType val)
        i <- case valType val of
            Typedef symbol -> do
                ObjField i <- look symbol (KeyTypeField typ)
                return i
            _ -> do
                let is = elemIndices (FieldType typ) fs
                assert (length is == 1) "ADT does not contain unique type field"
                return (head is)

        case base of
            _ | isNormalADT base -> do
                enumMatch <- mkIntInfix S.EqEq (mkI64 i) =<< adtEnum val
                b <- cmpPattern pat =<< adtDeref val i 0
                valLoad =<< mkInfix S.AndAnd enumMatch b
