{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Short      as BSS

import           Data.Maybe
import           Data.List
import           Control.Monad.Except hiding (void, fail)
import           Control.Monad.Trans
import           Control.Monad.Identity     
import           Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import           LLVM.AST                   hiding (function)
import           LLVM.AST.Global
import           LLVM.AST.Type              hiding (void)
import qualified LLVM.AST.Constant          as C
import           LLVM.IRBuilder.Instruction       
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Constant
import           LLVM.AST.IntegerPredicate
import           Foreign.Ptr
import qualified LLVM.Internal.FFI.DataLayout   as FFI
import           LLVM.Context

import qualified AST as S
import qualified Type as T
import qualified SymTab
import qualified Flatten as F
import Monad
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
            f = void $ func "main" [] VoidType $ \_ ->
                    cmp

            cmp :: (MonadFail m, Monad m, MonadIO m) => InstrCmpT CompileState m ()
            cmp = do
                forM_ (Map.toList $ F.typeDefs flatState) $ \(flat, (pos, typ)) -> cmpTypeDef flat pos typ
                mapM_ cmpVarDef (F.varDefs flatState)
                mapM_ cmpExternDef (F.externDefs flatState)
                mapM_ cmpFuncDef (F.funcDefs flatState)


cmpTypeDef :: InsCmp CompileState m => S.Symbol-> TextPos -> T.Type -> m ()
cmpTypeDef sym pos typ = do
    checkSymKeyUndef sym KeyType
    case typ of
        T.I8        -> addObj sym KeyType (ObType typ Nothing)
        T.I32       -> addObj sym KeyType (ObType typ Nothing)
        T.I64       -> addObj sym KeyType (ObType typ Nothing)
        T.Bool      -> addObj sym KeyType (ObType typ Nothing)
        T.Typedef f -> addObj sym KeyType (ObType typ Nothing)
        T.Tuple ts  -> do
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
    let name = mkName sym
    let paramTypes = map S.paramType params

    pushSymTab
    paramOpTypes <- forM params $ \(S.Param p s t) -> do
        checkSymKeyUndef s KeyVar
        addObj s KeyVar $ ObjVal (valBool False)
        opTypeOf t

    returnOpType <- maybe (return VoidType) opTypeOf mretty
    popSymTab

    addSymKeyDec sym (KeyFunc paramTypes) name (DecExtern paramOpTypes returnOpType False)
    let op = fnOp name paramOpTypes returnOpType False
    addObj sym (KeyFunc paramTypes) (ObjExtern paramTypes mretty op)


cmpFuncDef :: (MonadFail m, Monad m, MonadIO m) => S.Stmt -> InstrCmpT CompileState m ()
cmpFuncDef (S.Func pos sym params mretty blk) = do
    let paramTypes = map S.paramType params
    let symKey     = KeyFunc paramTypes
    checkSymKeyUndef sym symKey
    name@(Name nameStr) <- case sym of
        "main" -> return (mkName sym)
        _      -> freshName (mkBSS sym)

    pushSymTab
    (paramOpTypes, paramNames, paramSyms) <- fmap unzip3 $ forM params $ \(S.Param p s t) -> do
        opTyp <- opTypeOf t
        let paramName = mkBSS s
        return (opTyp, ParameterName paramName, s)

    returnOpType <- maybe (return VoidType) opTypeOf mretty

    curRetty <- gets curRetType
    modify $ \s -> s { curRetType = maybe T.Void id mretty }

    op <- InstrCmpT . IRBuilderT . lift $ func name (zip paramOpTypes paramNames) returnOpType $ \paramOps -> do
        (flip named) nameStr $ do
            forM_ (zip3 paramTypes paramOps paramSyms) $ \(typ, op, sym) -> do
                checkSymKeyUndef sym KeyVar
                addObj sym KeyVar (ObjVal (Ptr typ op))
            mapM_ cmpStmt blk

    modify $ \s -> s { curRetType = curRetty }

    popSymTab

    addObj sym symKey (ObjFunc mretty op) 
    addSymKeyDec sym (KeyFunc paramTypes) name (DecFunc paramOpTypes returnOpType)


cmpStmt :: InsCmp CompileState m => S.Stmt -> m ()
cmpStmt stmt = case stmt of
    S.Print pos exprs -> cmpPrint stmt

    S.CallStmt pos sym exprs -> do
        vals <- mapM cmpExpr exprs
        res <- look sym $ KeyFunc (map valType vals)
        op <- case res of
            ObjFunc _ op     -> return op
            ObjExtern _ _ op -> return op

        void $ call op [(o, []) | o <- map valOp vals]

    S.Assign pos (S.PatIdent p sym) expr -> do
        checkSymKeyUndef sym KeyVar
        val <- cmpExpr expr
        addObj sym KeyVar (ObjVal val)
    
    S.Return pos (Just expr) -> do
        val <- valLoad =<< cmpExpr expr
        typ <- baseTypeOf (valType val)

        val' <- case typ of
            T.Table _ -> valTableForceAlloc val
            _         -> return val

        retty <- gets curRetType
        checkTypesMatch (valType val') retty
        ret . valOp =<< valLoad val'

    _ -> error (show stmt)


cmpExpr :: InsCmp CompileState m =>  S.Expr -> m Value
cmpExpr expr = case expr of
    S.Call pos sym exprs -> do
        vals <- mapM cmpExpr exprs
        res <- look sym $ KeyFunc (map valType vals)
        (op, typ) <- case res of
            ObjFunc (Just typ) op     -> return (op, typ)
            ObjExtern _ (Just typ) op -> return (op, typ)

        fmap (Val typ) $ call op [(o, []) | o <- map valOp vals]
        

    S.Cons c -> case c of
        S.Int p n   -> return (valInt T.I64 n)
        S.Bool p b  -> return (valBool b)
        S.Char p c  -> return (valChar c)

    S.Ident p sym -> do
        ObjVal val <- look sym KeyVar
        return val

    S.Infix pos op exprA exprB -> do
        Val typA opA <- valLoad =<< cmpExpr exprA
        Val typB opB <- valLoad =<< cmpExpr exprB
        checkTypesMatch typA typB
        typ <- baseTypeOf typA
        cmpInfix op typ opA opB

    S.Conv pos typ [] -> zeroOf typ
        
    S.Table pos []     -> zeroOf (T.Table [])
    S.Table pos ([]:_) -> fail "cannot determine type of table row with no elements"
    S.Table pos exprss -> do
        valss <- mapM (mapM cmpExpr) exprss
        let rowLen = length (head valss)

        rowTypes <- forM valss $ \vals -> do
            assert (length vals == rowLen) $ "mismatched table row length of " ++ show (length vals)
            forM_ vals $ \val -> checkTypesMatch (valType val) $ valType (head vals)
            return $ valType (head vals)

        -- create local arrays to store table rows
        arrs <- mapM (valLocal . T.Array (fromIntegral rowLen)) rowTypes
        forM_ (zip arrs [0..]) $ \(arr, r) -> do
            forM_ [0..rowLen-1] $ \i -> do
                ptr <- valArrayIdx arr $ valInt T.I64 (fromIntegral i)
                valStore ptr ((valss !! r) !! i)

        tab <- valLocal (T.Table rowTypes)
        len <- valTableLen tab
        cap <- valTableCap tab

        valStore len $ valInt T.I64 (fromIntegral rowLen)
        valStore cap (valInt T.I64 0) -- shows stack mem

        forM_ (zip arrs [0..]) $ \(arr, i) ->
            valTableSetRow i tab =<< valArrayConstIdx arr 0

        return tab

    _ -> error ("expr: " ++ show expr)


cmpPrint :: InsCmp CompileState m => S.Stmt -> m ()
cmpPrint (S.Print pos exprs) = do
    prints =<< mapM cmpExpr exprs
    where
        prints :: InsCmp CompileState m => [Value] -> m ()
        prints []     = return ()
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs


cmpInfix :: InsCmp CompileState m => S.Op -> T.Type -> Operand -> Operand -> m Value
cmpInfix operator typ opA opB
    | T.isIntegral typ = case operator of
        S.Plus   -> res typ (add opA opB)
        S.Minus  -> res typ (sub opA opB)
        S.Times  -> res typ (mul opA opB)
        S.Divide -> res typ (sdiv opA opB)
        S.Mod    -> res typ (srem opA opB)
        S.LT     -> res T.Bool (icmp SLT opA opB)
        S.GT     -> res T.Bool (icmp SGT opA opB)
        S.LTEq   -> res T.Bool (icmp SLE opA opB)
        S.GTEq   -> res T.Bool (icmp SGT opA opB)
        _        -> fail ("no infix for " ++ show typ)
    where
        res typ = fmap (Val typ)


prettyCompileState :: CompileState -> IO ()
prettyCompileState state = do
    putStrLn "defs:"
    forM_ (definitions state) $ \def -> case def of
        TypeDefinition name mtyp ->
            putStrLn ("type: " ++ show name ++ " " ++ show mtyp)
        GlobalDefinition (Function _ _ _ _ _ retty name params _ _ _ _ _ _ basicBlocks _ _) -> do
            let ps = concat (map show $ fst params)
            putStrLn ("func: " ++ show name ++ " " ++ ps ++ " " ++ show retty)
        _ -> return ()
