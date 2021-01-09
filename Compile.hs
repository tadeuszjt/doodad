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
import           Control.Monad.Trans
import           Control.Monad.Identity     
import qualified Data.Set as Set
import qualified Data.Map as Map
import           LLVM.AST                   hiding (function)
import           LLVM.AST.Global
import           LLVM.AST.Constant          as C
import           LLVM.AST.Type              hiding (void)
import qualified LLVM.AST.Constant          as C
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Constant

import Monad
import Error
import qualified AST as S
import qualified Type as T
import qualified SymTab
import qualified Flatten as F
import Value2
import CompileState
import Print2
import Funcs

mkBSS = BSS.toShort . BS.pack

compileFlatState
    :: (Monad m, MonadFail m, MonadIO m)
    => Map.Map S.ModuleName CompileState
    -> F.FlattenState
    -> m (Either CmpError CompileState)
compileFlatState importCompiled flatState = do
    res <- runModuleCmpT emptyModuleBuilder (initCompileState { imports = importCompiled }) f
    case res of
        Left err                  -> return (Left err)
        Right (((), defs), state) -> return (Right state { definitions = defs })
    where
            f :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
            f = void $ function "main" [] VoidType $ \_ ->
                    getInstrCmp cmp

            cmp :: InsCmp CompileState m => m ()
            cmp = do
                forM_ (Map.toList $ F.typeDefs flatState) $ \(flat, (pos, typ)) -> cmpTypeDef flat pos typ
                mapM_ cmpVarDef (F.varDefs flatState)
                mapM_ cmpExternDef (F.externDefs flatState)
                mapM_ cmpFuncDef (F.funcDefs flatState)



opTypeOf :: ModCmp CompileState m => T.Type -> m Type
opTypeOf typ = case typ of
    T.I64       -> return i64
    T.Char      -> return i32
    T.I32       -> return i32
    T.Bool      -> return i1
    T.Tuple ts  -> fmap (StructureType False) (mapM opTypeOf ts)
    T.Typedef s -> do
        ObType t nm <- look s KeyType
        case nm of
            Nothing -> opTypeOf t
            Just n  -> return (NamedTypeReference n)
    _ -> error (show typ) 


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
        opTypeOf t
    returnOpType <- maybe (return VoidType) opTypeOf mretty
    popSymTab

    addSymKeyDec sym (KeyFunc paramTypes) name (DecExtern paramOpTypes returnOpType False)


cmpFuncDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpFuncDef (S.Func pos sym params mretty blk) = do
    let paramTypes = map S.paramType params
    let symKey     = KeyFunc paramTypes
    checkSymKeyUndef sym symKey
    name <- freshName (mkBSS sym)

    pushSymTab
    (paramOpTypes, paramNames, paramSyms) <- fmap unzip3 $ forM params $ \(S.Param p s t) -> do
        opTyp <- opTypeOf t
        let paramName = mkBSS s
        return (opTyp, ParameterName paramName, s)

    returnOpType <- maybe (return VoidType) opTypeOf mretty

    --TODO This doesn't work
    op <- function name (zip paramOpTypes paramNames) returnOpType $ \paramOps -> lift $ do
        forM_ (zip3 paramTypes paramOps paramSyms) $ \(typ, op, sym) -> do
            checkSymKeyUndef sym KeyVar
            addObj sym KeyVar (ObjVal (Ptr typ op))
        mapM_ cmpStmt blk
    
    popSymTab

    addObj sym symKey (ObjFunc mretty op) 
    addSymKeyDec sym (KeyFunc paramTypes) name (DecFunc paramOpTypes returnOpType)


cmpStmt :: InsCmp CompileState m => S.Stmt -> m ()
cmpStmt stmt = case stmt of
    S.Print pos exprs -> do
        vals <- mapM cmpExpr exprs
        mapM_ (valPrint ", ") vals
        printf ("\n") []
        

        return ()
    _ -> fail (show stmt)



cmpExpr :: InsCmp CompileState m =>  S.Expr -> m Value
cmpExpr expr = case expr of
    S.Cons c -> case c of
        S.Int p n   -> return (valInt T.I64 n)
        S.Bool p b  -> return (valBool b)
    S.Ident p s -> do ObjVal v <- look s KeyVar; return v
    _ -> fail (show expr)



prettyCompileState :: CompileState -> IO ()
prettyCompileState state = do
    putStrLn "defs:"
    forM_ (definitions state) $ \def -> case def of
        TypeDefinition name mtyp ->
            putStrLn ("type: " ++ show name ++ " " ++ show mtyp)
        GlobalDefinition (Function _ _ _ _ _ retty name params _ _ _ _ _ _ basicBlocks _ _) -> do
            let ps = concat (map show $ fst params)
            putStrLn ("func: " ++ show name ++ " " ++ ps ++ " " ++ show retty)
            forM_ basicBlocks $ \bb -> do
                putStrLn ("\t" ++ "block:")
