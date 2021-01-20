{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Fail hiding (fail)
import Control.Monad.Except hiding (void, fail)
import Foreign.Ptr

import qualified LLVM.AST as LL
import qualified LLVM.AST.Type as LL
import qualified LLVM.Internal.FFI.DataLayout as FFI
import LLVM.AST.Global
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Context

import qualified AST as S
import qualified Flatten as F
import Monad
import Type
import Error
import Value
import CompileState
import Print
import Funcs
import Table

mkBSS = BSS.toShort . BS.pack

compileFlatState
    :: BoM s m
    => Context
    -> Ptr FFI.DataLayout
    -> Map.Map S.ModuleName CompileState
    -> F.FlattenState
    -> m CompileState
compileFlatState ctx dl importCompiled flatState = do
    let initState = (initCompileState ctx dl) { imports = importCompiled }
    res <- runBoMT initState (runModuleCmpT emptyModuleBuilder f)
    case res of
        Left err                  -> throwError err
        Right (((), defs), state) -> return $ state { definitions = defs }
    where
            f :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
            f = void $ func "main" [] LL.VoidType $ \_ ->
                    cmp

            cmp :: (MonadFail m, Monad m, MonadIO m) => InstrCmpT CompileState m ()
            cmp = do
                forM_ (Map.toList $ F.typeDefs flatState) $ \(flat, (pos, typ)) -> cmpTypeDef flat pos typ
                mapM_ cmpVarDef (F.varDefs flatState)
                mapM_ cmpExternDef (F.externDefs flatState)
                mapM_ cmpFuncDef (F.funcDefs flatState)


cmpTypeDef :: InsCmp CompileState m => S.Symbol-> TextPos -> Type -> m ()
cmpTypeDef sym pos typ = do
    checkSymKeyUndef sym KeyType
    case typ of
        I8        -> addObj sym KeyType (ObType typ Nothing)
        I32       -> addObj sym KeyType (ObType typ Nothing)
        I64       -> addObj sym KeyType (ObType typ Nothing)
        Bool      -> addObj sym KeyType (ObType typ Nothing)
        Typedef f -> addObj sym KeyType (ObType typ Nothing)
        Tuple ts  -> do
            name <- freshName (mkBSS sym)
            opTyp <- opTypeOf typ
            typedef name (Just opTyp)
            addDeclared name
            addSymKeyDec sym KeyType name DecType
            addObj sym KeyType $ ObType typ (Just name)
        _ -> error (show typ)
    return ()


cmpVarDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpVarDef (S.Assign pos pat expr) = do
    return ()


cmpExternDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpExternDef (S.Extern pos sym params mretty) = do
    checkSymUndef sym 
    let name = LL.mkName sym
    let paramTypes = map S.paramType params

    pushSymTab
    paramOpTypes <- forM params $ \(S.Param p s t) -> do
        checkSymKeyUndef s KeyVar
        addObj s KeyVar $ ObjVal (valBool False)
        opTypeOf t

    returnOpType <- maybe (return LL.VoidType) opTypeOf mretty
    popSymTab

    addSymKeyDec sym (KeyFunc paramTypes) name (DecExtern paramOpTypes returnOpType False)
    let op = fnOp name paramOpTypes returnOpType False
    addObj sym (KeyFunc paramTypes) (ObjExtern paramTypes mretty op)


cmpFuncDef :: (MonadFail m, Monad m, MonadIO m) => S.Stmt -> InstrCmpT CompileState m ()
cmpFuncDef (S.Func pos sym params mretty blk) = withPos pos $ do
    let paramTypes = map S.paramType params
    let symKey     = KeyFunc paramTypes
    checkSymKeyUndef sym symKey
    name@(LL.Name nameStr) <- case sym of
        "main" -> return (LL.mkName sym)
        _      -> freshName (mkBSS sym)

    (paramOpTypes, paramNames, paramSyms) <- fmap unzip3 $ forM params $ \(S.Param p s t) -> do
        opTyp <- opTypeOf t
        let paramName = mkBSS s
        return (opTyp, ParameterName paramName, s)

    returnOpType <- maybe (return LL.VoidType) opTypeOf mretty

    let op = fnOp name paramOpTypes returnOpType False
    addObj sym symKey (ObjFunc mretty op) 
    addSymKeyDec sym (KeyFunc paramTypes) name (DecFunc paramOpTypes returnOpType)
    addDeclared name

    pushSymTab
    curRetty <- gets curRetType
    modify $ \s -> s { curRetType = maybe Void id mretty }
    void $ InstrCmpT . IRBuilderT . lift $ func name (zip paramOpTypes paramNames) returnOpType $ \paramOps -> do
        (flip named) nameStr $ do
            forM_ (zip3 paramTypes paramOps paramSyms) $ \(typ, op, sym) -> do
                checkSymKeyUndef sym KeyVar
                loc <- valLocal typ
                valStore loc (Val typ op)
                addObj sym KeyVar (ObjVal loc)

            mapM_ cmpStmt blk

            retTyp <- gets curRetType
            hasTerm <- hasTerminator
            unless hasTerm $
                if retTyp == Void
                then retVoid
                else ret . valOp =<< zeroOf retTyp

    modify $ \s -> s { curRetType = curRetty }
    popSymTab


cmpStmt :: InsCmp CompileState m => S.Stmt -> m ()
cmpStmt stmt = case stmt of
    S.Print pos exprs -> cmpPrint stmt

    S.CallStmt pos sym exprs -> withPos pos $ do
        vals <- mapM valLoad =<< mapM cmpExpr exprs
        res <- look sym $ KeyFunc (map valType vals)
        op <- case res of
            ObjFunc _ op     -> return op
            ObjExtern _ _ op -> return op

        void $ call op [(o, []) | o <- map valOp vals]

    S.Assign pos pat expr -> withPos pos $ do
        val <- cmpExpr expr
        matched <- cmpPattern pat val
        if_ (valOp matched) (return ()) (void trap) 

    S.Set pos ind expr -> withPos pos $ do
        val <- cmpExpr expr

        case ind of
            S.IndIdent p sym -> do
                ObjVal loc <- look sym KeyVar
                valStore loc val
        
    S.Return pos Nothing -> withPos pos $ do
        curRetty <- gets curRetType
        assert (curRetty == Void) "must return a value"
        retVoid
        emitBlockStart =<< fresh

    S.Return pos (Just expr) -> withPos pos $ do
        val <- cmpExpr expr
        typ <- valBaseType val

        val' <- case typ of
            Table _ -> tableForceAlloc val
            _         -> return val

        retty <- gets curRetType
        checkTypesMatch (valType val') retty
        ret . valOp =<< valLoad val'
        emitBlockStart =<< fresh

    S.If pos expr blk melse -> withPos pos $ do
        val <- cmpExpr expr
        typ <- valBaseType val
        checkTypesMatch typ Bool

        case melse of
            Nothing -> if_ (valOp val) (cmpStmt blk) (return ())

    S.While pos expr blk -> withPos pos $ do
        cond <- freshName "while_cond"
        body <- freshName "while_body"
        exit <- freshName "while_exit"

        br cond
        emitBlockStart cond
        cnd <- valLoad =<< cmpExpr expr
        checkTypesMatch Bool =<< valBaseType cnd
        condBr (valOp cnd) body exit
        
        emitBlockStart body
        pushSymTab
        mapM_ cmpStmt blk
        popSymTab
        br cond
        emitBlockStart exit

    S.Switch pos expr cases -> withPos pos $ do
        val <- cmpExpr expr
        casesM <- forM cases $ \(pat, stmt) -> do
            let b = return . valOp =<< valLoad =<< cmpPattern pat val
            let s = cmpStmt stmt
            return (b, s)

        switch_ casesM

    S.Block pos stmts -> withPos pos $ do
        pushSymTab
        mapM_ cmpStmt stmts
        popSymTab


    _ -> error (show stmt)


cmpExpr :: InsCmp CompileState m =>  S.Expr -> m Value
cmpExpr expr = case expr of
    S.Call pos sym exprs -> withPos pos $ do
        vals <- mapM valLoad =<< mapM cmpExpr exprs
        res <- look sym $ KeyFunc (map valType vals)
        (op, typ) <- case res of
            ObjFunc (Just typ) op     -> return (op, typ)
            ObjExtern _ (Just typ) op -> return (op, typ)

        fmap (Val typ) $ call op [(o, []) | o <- map valOp vals]
        
    S.Cons c -> case c of
        S.Int pos n  -> return (valI64 n)
        S.Bool pos b -> return (valBool b)
        S.Char pos c -> return (valChar c)

    S.Ident pos sym -> withPos pos $ do
        ObjVal val <- look sym KeyVar
        return val

    S.Infix pos op exprA exprB -> withPos pos $ do
        a <- cmpExpr exprA
        b <- cmpExpr exprB
        valsInfix op a b

    S.Conv pos typ [] ->
        zeroOf typ

    S.Len pos expr -> withPos pos $ do
        val <- cmpExpr expr
        typ <- valBaseType val
        case typ of
            Table _ -> tableLen val
            _       -> fail $ "cannot take length of type: " ++ show (valType val)

    S.Tuple pos exprs -> withPos pos $ do
        vals <- mapM cmpExpr exprs
        tup <- valLocal $ Tuple (map valType vals)
        forM_ (zip vals [0..]) $ \(val, i) -> do
            valTupleSet tup i val

        return tup

    S.Subscript pos aggExpr idxExpr -> withPos pos $ do
        agg <- cmpExpr aggExpr
        idx <- cmpExpr idxExpr

        idxType <- valBaseType idx
        aggType <- valBaseType agg

        assert (isInt idxType) "index type isn't an integer"

        case aggType of
            Table [t] -> do
                tup <- tableGetElem agg idx
                valTupleIdx tup 0


    S.Table pos []     -> zeroOf (Table [])
    S.Table pos ([]:_) -> withPos pos $ fail "cannot determine type of table row with no elements"
    S.Table pos exprss -> withPos pos $ do
        valss <- mapM (mapM cmpExpr) exprss
        let rowLen = length (head valss)

        rowTypes <- forM valss $ \vals -> do
            assert (length vals == rowLen) $ "mismatched table row length of " ++ show (length vals)
            forM_ vals $ \val -> checkTypesMatch (valType val) $ valType (head vals)
            return $ valType (head vals)

        -- create local arrays to store table rows
        arrs <- mapM (valLocal . Array (fromIntegral rowLen)) rowTypes
        forM_ (zip arrs [0..]) $ \(arr, r) -> do
            forM_ [0..rowLen-1] $ \i -> do
                ptr <- valArrayIdx arr $ valI64 (fromIntegral i)
                valStore ptr ((valss !! r) !! i)

        tab <- valLocal (Table rowTypes)
        len <- tableLen tab
        cap <- tableCap tab

        valStore len $ valI64 (fromIntegral rowLen)
        valStore cap (valI64 0) -- shows stack mem

        forM_ (zip arrs [0..]) $ \(arr, i) ->
            tableSetRow tab i =<< valArrayConstIdx arr 0

        return tab

    S.Append pos expr elem -> withPos pos $ do
        tab <- cmpExpr expr
        val <- cmpExpr elem
        typ <- valBaseType val
        case typ of
            Tuple _ -> tableAppend tab val
            _         -> do
                tup <- valLocal (Tuple [valType val])
                valTupleSet tup 0 val
                tableAppend tab tup

    _ -> error ("expr: " ++ show expr)


cmpPattern :: InsCmp CompileState m => S.Pattern -> Value -> m Value
cmpPattern pat val = case pat of
    S.PatIgnore pos    -> return (valBool True)
    S.PatLiteral cons  -> valsInfix S.EqEq val =<< cmpExpr (S.Cons cons)
    S.PatIdent pos sym -> withPos pos $ do
        checkSymKeyUndef sym KeyVar
        loc <- valLocal (valType val)
        addObj sym KeyVar (ObjVal loc)
        valStore loc val
        return (valBool True)
        

cmpPrint :: InsCmp CompileState m => S.Stmt -> m ()
cmpPrint (S.Print pos exprs) = withPos pos $ do
    prints =<< mapM cmpExpr exprs
    where
        prints :: InsCmp CompileState m => [Value] -> m ()
        prints []     = return ()
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs
