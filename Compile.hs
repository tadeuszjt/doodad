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
    -> Map.Map S.Path CompileState
    -> F.FlattenState
    -> m CompileState
compileFlatState ctx dl imports flatState = do
    res <- runBoMT (initCompileState ctx dl imports) (runModuleCmpT emptyModuleBuilder f)
    case res of
        Left err                 -> throwError err
        Right ((_, defs), state) -> return $ state { definitions = defs }
    where
            f :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
            f = void $ func "main" [] LL.VoidType $ \_ ->
                    cmp

            cmp :: (MonadFail m, Monad m, MonadIO m) => InstrCmpT CompileState m ()
            cmp = do
                forM_ (Map.toList $ F.typeDefs flatState) $ \(flat, (pos, typ)) -> cmpTypeDef (S.Typedef pos flat typ)
                mapM_ cmpVarDef (F.varDefs flatState)
                mapM_ cmpExternDef (F.externDefs flatState)
                mapM_ cmpFuncDef (F.funcDefs flatState)


cmpTypeDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpTypeDef (S.Typedef pos sym typ) = withPos pos $ do
    checkSymKeyUndef sym KeyType
    checkSymKeyUndef sym (KeyFunc [])
    checkSymKeyUndef sym (KeyFunc [typ])
    checkSymKeyUndef sym (KeyFunc [Typedef sym])
    addObj sym (KeyFunc [])            (ObjConstructor (Typedef sym))
    addObj sym (KeyFunc [typ])         (ObjConstructor (Typedef sym))
    addObj sym (KeyFunc [Typedef sym]) (ObjConstructor (Typedef sym))

    case typ of
        Tuple ts -> do
            name <- freshName (mkBSS sym)
            opTyp <- opTypeOf typ
            typedef name (Just opTyp)
            addDeclared name
            addSymKeyDec sym KeyType name DecType
            addObj sym KeyType $ ObType typ (Just name)


        Pointer ts -> do
            forM_ ts $ \t -> do
                addObj sym (KeyFunc [Pointer [t]]) (ObjConstructor (Typedef sym))
            addObj sym KeyType (ObType typ Nothing)

        _ -> addObj sym KeyType (ObType typ Nothing)


cmpVarDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpVarDef (S.Assign pos (S.PatIdent p sym) expr) = do
    val <- cmpExpr expr

    checkSymKeyUndef sym KeyVar
    name <- freshName (mkBSS sym)
    
    let typ = valType val
    opTyp <- opTypeOf typ
    initialiser <- zeroOf typ
    loc <- fmap (Ptr typ) $ global name opTyp (toCons $ valOp initialiser)
    valStore loc val

    addObj sym KeyVar (ObjVal loc)
    addSymKeyDec sym KeyVar name (DecVar opTyp)
    addDeclared name


cmpExternDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpExternDef (S.Extern pos sym params retty) = do
    checkSymUndef sym 
    let name = LL.mkName sym
    let paramTypes = map S.paramType params

    pushSymTab
    paramOpTypes <- forM params $ \(S.Param p s t) -> do
        checkSymKeyUndef s KeyVar
        addObj s KeyVar $ ObjVal (valBool False)
        opTypeOf t

    returnOpType <- opTypeOf retty
    popSymTab

    addSymKeyDec sym (KeyFunc paramTypes) name (DecExtern paramOpTypes returnOpType False)
    let op = fnOp name paramOpTypes returnOpType False
    addObj sym (KeyFunc paramTypes) (ObjExtern paramTypes retty op)


cmpFuncDef :: (MonadFail m, Monad m, MonadIO m) => S.Stmt -> InstrCmpT CompileState m ()
cmpFuncDef (S.Func pos "main" params retty blk) = withPos pos $ do
    assert (params == [])  "main cannot have parameters"
    assert (retty == Void) "main must return void"
    pushSymTab >> mapM_ cmpStmt blk >> popSymTab
cmpFuncDef (S.Func pos sym params retty blk) = withPos pos $ do
    let paramTypes = map S.paramType params
    let symKey     = KeyFunc paramTypes
    checkSymKeyUndef sym symKey
    name@(LL.Name nameStr) <- freshName (mkBSS sym)

    (paramOpTypes, paramNames, paramSyms) <- fmap unzip3 $ forM params $ \(S.Param p s t) -> do
        opTyp <- opTypeOf t
        let paramName = mkBSS s
        return (opTyp, ParameterName paramName, s)

    returnOpType <- opTypeOf retty

    let op = fnOp name paramOpTypes returnOpType False
    addObj sym symKey (ObjFunc retty op) 
    addSymKeyDec sym (KeyFunc paramTypes) name (DecFunc paramOpTypes returnOpType)
    addDeclared name

    pushSymTab
    curRetty <- gets curRetType
    modify $ \s -> s { curRetType = retty }
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
        retty <- gets curRetType
        ret . valOp =<< valLoad =<< valAsType retty =<< cmpExpr expr
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


-- must return Val unless local variable
cmpExpr :: InsCmp CompileState m =>  S.Expr -> m Value
cmpExpr expr = case expr of
    S.Int pos n    -> return (valI64 n)
    S.Bool pos b   -> return (valBool b)
    S.Char pos c   -> return (valChar c)
            
    S.String pos s -> do
        loc <- globalStringPtr s =<< fresh
        fmap (Val String) $ bitcast (cons loc) (LL.ptr LL.i8)

    S.Ident pos sym -> withPos pos $ do
        ObjVal loc <- look sym KeyVar
        return loc

    S.Call pos sym exprs -> withPos pos $ do
        vals <- mapM valLoad =<< mapM cmpExpr exprs
        res <- look sym $ KeyFunc (map valType vals)
        case res of
            ObjConstructor typ -> valConstruct typ vals
            _                  -> do
                (op, typ) <- case res of
                    ObjFunc Void _     -> err "cannot use void function as expression"
                    ObjExtern _ Void _ -> err "cannot use void function as expression"
                    ObjFunc typ op     -> return (op, typ)
                    ObjExtern _ typ op -> return (op, typ)

                fmap (Val typ) $ call op [(o, []) | o <- map valOp vals]

    S.Infix pos op exprA exprB -> withPos pos $ do
        a <- cmpExpr exprA
        b <- cmpExpr exprB
        valsInfix op a b

    S.Conv pos typ [] ->
        zeroOf typ

    S.Len pos expr -> withPos pos $ valLoad =<< do
        val <- cmpExpr expr
        typ <- valBaseType val
        case typ of
            Table _ -> tableLen val
            _       -> err ("cannot take length of type " ++ show typ)

    S.Tuple pos exprs -> withPos pos $ valLoad =<< do
        vals <- mapM cmpExpr exprs
        if any valContextual vals
        then return (CtxTuple vals)
        else do
            tup <- valLocal $ Tuple (map valType vals)
            zipWithM_ (valTupleSet tup) [0..] vals
            return tup

    S.Subscript pos aggExpr idxExpr -> withPos pos $ valLoad =<< do
        agg <- cmpExpr aggExpr
        idx <- cmpExpr idxExpr

        idxType <- valBaseType idx
        aggType <- valBaseType agg

        assert (isInt idxType) "index type isn't an integer"

        case aggType of
            String    -> do
                Val _ idxOp    <- valLoad idx
                Val _ loc <- valLoad agg
                p <- gep loc [idxOp]
                e <- load p 0
                c <- sext e LL.i32
                return (Val Char c)
                
            Table [t] -> do
                tup <- tableGetElem agg idx
                valTupleIdx tup 0

    S.Address pos expr -> withPos pos $ do
        val <- cmpExpr expr
        case val of
            Ptr t loc -> return $ Val (Pointer [t]) loc
            Val t _   -> do
                mal <- valMalloc t (valI64 1)
                valStore mal val
                return $ Val (Pointer [t]) (valLoc mal) 
        
    S.Table pos ([]:rs) -> withPos pos $ do
        assert (all null rs) "row lengths do not match"
        return (CtxTable [[]])
    S.Table pos exprss -> withPos pos $ valLoad =<< do
        valss <- mapM (mapM cmpExpr) exprss
        let rowLen = length (head valss)

        rowTypes <- forM valss $ \vals -> do
            assert (length vals == rowLen) $ "mismatched table row length of " ++ show (length vals)
            forM_ vals $ \val -> checkTypesMatch (valType val) $ valType (head vals)
            return $ valType (head vals)

        rows <- forM (zip rowTypes [0..]) $ \(t, r) -> do
            mal <- valMalloc t $ valI64 (fromIntegral rowLen)
            forM_ [0..rowLen-1] $ \i -> do
                ptr <- valPtrIdx mal $ valI64 (fromIntegral i) 
                valStore ptr ((valss !! r) !! i)
            return mal

        tab <- valLocal (Table rowTypes)
        len <- tableLen tab
        cap <- tableCap tab

        valStore len $ valI64 (fromIntegral rowLen)
        valStore cap len

        zipWithM_ (tableSetRow tab) [0..] rows
        return tab

    S.Append pos expr elem -> withPos pos $ valLoad =<< do
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
    S.PatLiteral expr  -> valsInfix S.EqEq val =<< cmpExpr expr
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