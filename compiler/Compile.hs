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
import Builtin
import Symbol


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
    initialiser <- valZero typ
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
        initialiser <- valZero typ
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
                    loc <- valLocal typ
                    valStore loc (Val typ op)
                    define symbol KeyVar (ObjVal loc)
                cmpFnBlock
        Nothing -> do
            ObjFnOp op <- look (Sym sym) (KeyFunc argTypes retty)

            let LL.ConstantOperand (C.GlobalReference _ name) = op
            let Name nameStr = name
            void $ InstrCmpT . IRBuilderT . lift $ func name (zip argOpTypes argNames) returnOpType $ \argOps -> do
                forM_ (zip3 argTypes argOps argSymbols) $ \(typ, op, symbol) -> do
                    loc <- valLocal typ
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




cmpInfix :: InsCmp CompileState m => Type -> S.Op -> Value -> Value -> m Value
cmpInfix typ op valA valB = do
    mobj <- lookm (Sym $ show op) $ KeyFunc [valType valA, valType valB] typ
    case mobj of
        Just (ObjFnOp op) -> do
            opA <- valOp <$> valLoad valA
            opB <- valOp <$> valLoad valB
            Val typ <$> call op [(opA, []), (opB, [])]
        Nothing -> valsInfix op valA valB


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


cmpStmt :: InsCmp CompileState m => S.Stmt -> m ()
cmpStmt stmt = trace "cmpStmt" $ withPos stmt $ case stmt of
    S.Print pos exprs   -> do
        label "print"
        cmpPrint stmt

    S.Block stmts       -> mapM_ cmpStmt stmts

    S.ExprStmt expr     -> void $ cmpExpr expr

    S.Assign pos pat expr -> trace ("assign " ++ show pat) $ do
        label "assign"
        val <- cmpExpr expr
        isDataType <- isDataType (valType val)
        assert (not isDataType) "Cannot use a data type for a variable"
        matched <- valLoad =<< cmpPattern pat val
        if_ (valOp matched) (return ()) (void trap) 

    S.Set pos expr1 expr2 -> do
        label "set"
        loc@(Ptr _ _) <- cmpExpr expr1
        valStore loc =<< cmpExpr expr2

    S.Return pos Nothing -> do
        label "return"
        retVoid
        emitBlockStart =<< fresh

    S.Return pos (Just expr) -> do
        label "return_expr"
        ret . valOp =<< valLoad =<< cmpExpr expr
        emitBlockStart =<< fresh

    S.If pos cnd blk melse -> do
        label "if"
        val <- valLoad =<< cmpCondition cnd
        assertBaseType (== Bool) (valType val)
        if_ (valOp val) (cmpStmt blk) $ maybe (return ()) cmpStmt melse

    S.While pos cnd blk -> do
        label "while"
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
        label "switch"
        val <- cmpExpr expr
        let cases' = [(fmap valOp (cmpPattern pat val), cmpStmt stmt) | (pat, stmt) <- cases]
        switch_ $ cases' ++ [(return (bit 1), void trap)]

    S.For _ symbol (Just typ) expr mpat blk -> do
        label "for"
        idx <- valLocal typ
        valStore idx =<< valZero typ
        define symbol KeyVar (ObjVal idx) 

        val <- cmpExpr expr
        base <- baseTypeOf (valType val)

        cond <- freshName "for_cond"
        body <- freshName "for_body"
        patm <- freshName "for_patm"
        incr <- freshName "for_incr"
        exit <- freshName "for_exit"

        br cond
        emitBlockStart cond

        len <- case base of
            Table ts  -> tableLen val
            Array n t -> return $ valI64 n
            String    -> valStringLen I64 val
        ilt <- valsInfix S.LT idx len
        condBr (valOp ilt) patm exit

        emitBlockStart patm
        patMatch <- case mpat of
            Nothing -> valBool Bool True
            Just pat -> case base of
                Table ts  -> cmpPattern pat =<< tableGetElem val idx
                Array n t -> cmpPattern pat =<< arrayGetElem val idx
        condBr (valOp patMatch) body exit
        
        emitBlockStart body
        cmpStmt blk
        br incr

        emitBlockStart incr
        valStore idx =<< valsInfix S.Plus idx =<< valInt typ 1
        br cond

        emitBlockStart exit

    _ -> error "stmt"
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
    S.Bool pos b               -> valBool exprType b
    S.Char pos c               -> valChar exprType c
    S.Conv pos typ [expr]      -> valConvert typ =<< cmpExpr expr
    S.Tuple pos [expr]         -> cmpExpr expr
    S.Float p f                -> valFloat exprType f
    S.Prefix pos operator expr -> valPrefix operator =<< cmpExpr expr
    S.Field pos expr sym      -> tupleField sym =<< cmpExpr expr
    S.Null p                   -> adtNull exprType

    S.Push pos expr [expr2] -> do
        loc@(Ptr _ _) <- cmpExpr expr
        val <- cmpExpr expr2
        len <- tableLen loc
        tableAppendElem loc val
        valLoad len

    S.Push pos expr [] -> do
        loc@(Ptr _ _) <- cmpExpr expr
        Table [t] <- assertBaseType isTable (valType loc)
        len <- tableLen loc
        tableAppendElem loc =<< valZero t
        valConvertNumber exprType =<< valLoad len

    S.Pop pos expr [] -> do
        loc@(Ptr _ _) <- cmpExpr expr
        tablePopElem loc

    S.Clear pos expr -> do
        assert (exprType == Void) "clear is a void expression"
        tab <- cmpExpr expr
        tableClear tab
        return $ Val Void (valLoc tab)

    S.Int p n -> do
        base <- baseTypeOf exprType
        case base of
            _ | isInt base   -> valInt exprType n
            _ | isFloat base -> valFloat exprType (fromIntegral n)
            _ | base == Char -> valChar exprType (chr $ fromIntegral n)
            _ | otherwise    -> fail $ "invalid base type of: " ++ show base

    S.Infix pos op exprA exprB -> do
        valA <- cmpExpr exprA
        valB <- cmpExpr exprB
        cmpInfix exprType op valA valB

    S.Ident pos symbol -> do
        obj <- look symbol KeyVar
        case obj of
            ObjVal (ConstInt n) -> valInt I64 n
            ObjVal loc -> return loc
            
    S.String pos s -> do
        assertBaseType (== String) exprType
        loc <- globalStringPtr s =<< myFreshPrivate "str"
        return (Val exprType $ cons loc)

    S.CallMember pos expr symbol exprs  -> do
        val <- cmpExpr expr
        -- TODO possible to cause incorrect logic?
        loc <- case val of
            Val _ op -> do 
                local <- valLocal (valType val)
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
            ObjConstructor  -> valConstruct exprType vals
            ObjFnOp op      -> Val exprType <$> call op [(o, []) | o <- map valOp vals]
            ObjField i      -> adtConstructField symbol exprType vals
    
    S.Len pos expr -> valLoad =<< do
        assertBaseType isInt exprType
        val <- cmpExpr expr
        base <- baseTypeOf (valType val)
        case base of
            Table _   -> valConvertNumber exprType =<< tableLen val
            Array n t -> valInt exprType n
            String    -> valStringLen exprType val
            _       -> fail ("cannot take length of type " ++ show (valType val))

    S.Tuple pos exprs -> do
        Tuple ts <- assertBaseType isTuple exprType
        vals <- mapM cmpExpr exprs
        assert (ts == map valType vals) $
            "Incorrect val types: " ++ show ts ++ ", " ++ show (map valType vals)

        tup <- valLocal exprType
        zipWithM_ (tupleSet tup) [0..] vals
        return tup

    S.TupleIndex pos expr n -> do
        val <- cmpExpr expr
        tupleIdx (fromIntegral n) val

    S.Subscript pos expr idxExpr -> do
        val <- cmpExpr expr
        idx <- cmpExpr idxExpr
        base <- baseTypeOf (valType val)
        case base of
            Table _   -> tableGetElem val idx
            Array _ _ -> arrayGetElem val idx
            String    -> valStringIdx val idx 

    S.Table pos exprss -> valLoad =<< do
        base <- baseTypeOf exprType
        case base of
            Array n t -> do
                assert (length exprss == 1) "Arrays cannot have multiple rows"
                vals <- mapM cmpExpr (head exprss)
                assert (length vals == n) "Invalid array length"
                arr <- valLocal exprType
                forM_ (zip vals [0..]) $ \(val, i) -> do
                    arraySetElem arr (valI64 i) val
                return arr
                
            Table ts -> do
                valss <- mapM (mapM cmpExpr) exprss
                tab <- tableMake exprType (valI64 $ length $ head valss)

                forM (zip valss [0..]) $ \(vals, r) -> do
                    assert (length vals == (length $ head valss)) "Mismatched row length"
                    row <- tableRow r tab
                    forM (zip vals [0..]) $ \(val, c) -> do
                        ptr <- valPtrIdx row (valI64 c)
                        valStore ptr val
                return tab

            _ -> fail $ "invalid table base type: " ++ show base

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
        mal <- valMalloc (valType val) (valI64 1)
        valStore mal =<< valCopy val
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
cmpPattern pattern val = trace "cmpPattern" $ withPos pattern $ case pattern of
    S.PatIgnore _     -> valBool Bool True
    S.PatLiteral expr -> cmpInfix Bool S.EqEq val =<< cmpExpr expr

    S.PatNull _ -> do
        base@(ADT fs) <- assertBaseType isADT (valType val)
        let is = elemIndices FieldNull fs
        assert (length is == 1) "ADT type does not have unique null field"
        let [i] = is
        valIntInfix S.EqEq (valI64 i) =<< adtEnum val

    S.PatAnnotated pat typ -> do
        assert (valType val == typ) "pattern type mismatch"
        cmpPattern pat val

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

            Array n t -> do
                assert (n == length pats) "Invalid array pattern"
                bs <- forM (zip pats [0..]) $ \(p, i) ->
                    cmpPattern p =<< arrayGetElemConst val i

                true <- valBool Bool True
                foldM (valsInfix S.AndAnd) true bs
            
            _ -> fail "Invalid array pattern"

    S.PatField _ symbol pats -> do
        base@(ADT fs) <- assertBaseType isADT (valType val)
        obj <- look symbol (KeyField $ valType val)
        case obj of
            ObjAdtTypeField i -> do
                assert (length pats == 1)       "One pattern allowed for type field"
                enumMatch <- valIntInfix S.EqEq (valI64 i) =<< adtEnum val
                Ptr _ loc <- adtDeref val i 0
                ObType t0 _ <- look symbol KeyType
                b <- cmpPattern (head pats) $ Ptr t0 loc
                valLoad =<< valsInfix S.AndAnd enumMatch b


            ObjField i -> do
                let FieldCtor ts = fs !! i
                assert (length pats == length ts) "invalid ADT pattern"

                case base of
                    _ | isEnumADT base -> do
                        assert (length pats == 0) "invalid ADT pattern"
                        valIntInfix S.EqEq (valI64 i) =<< adtEnum val

                    _ | isNormalADT base -> do
                        enumMatch <- valIntInfix S.EqEq (valI64 i) =<< adtEnum val
                        -- can't be inside a block which may or may not happen
                        -- as cmpPattern may add variables to the symbol table
                        bs <- forM (zip pats [0..]) $ \(pat, j) -> 
                            cmpPattern pat =<< adtDeref val i j

                        valLoad =<< foldM (valsInfix S.AndAnd) enumMatch bs


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
                enumMatch <- valIntInfix S.EqEq (valI64 i) =<< adtEnum val
                b <- cmpPattern pat =<< adtDeref val i 0
                valLoad =<< valsInfix S.AndAnd enumMatch b
