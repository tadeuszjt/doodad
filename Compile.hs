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
import           Control.Monad.Except       hiding (void)
import           Control.Monad.State        hiding (void)
import           Control.Monad.Trans
import           Control.Monad.Fail         hiding (fail)
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

import Monad
import Error
import qualified AST as S
import qualified Type as T
import qualified SymTab
import qualified Flatten as F

mkBSS = BSS.toShort . BS.pack

data SymKey
    = KeyType
    | KeyVar
    | KeyFunc
    deriving (Show, Eq, Ord)


data Object
    = ObType    T.Type   (Maybe Name)
    | ObjExtern [T.Type] (Maybe T.Type) Operand
    | ObjFunc   [T.Type] (Maybe T.Type) Operand
    | ObjVal
    deriving (Show)


data Declaration
    = DecType   Name 
    | DecExtern Name [Type] Type Bool
    | DecFunc   Name [Type] Type
    deriving (Show)


data CompileState
    = CompileState
        { imports      :: Map.Map S.ModuleName CompileState
        , declarations :: Map.Map F.FlatSym Declaration
        , definitions  :: [Definition]
        , declared     :: Set.Set F.FlatSym
        , objTab       :: Map.Map F.FlatSym Object
        }
    deriving (Show)

initCompileState
     = CompileState
        { imports      = Map.empty
        , declarations = Map.empty
        , definitions  = []
        , declared     = Set.empty
        , objTab       = Map.empty
        }



addObj :: BoM CompileState m => F.FlatSym -> Object -> m ()
addObj flat obj =
    modify $ \s -> s { objTab = Map.insert flat obj (objTab s) }


addDeclared :: BoM CompileState m => F.FlatSym -> m ()
addDeclared flat =
    modify $ \s -> s { declared = Set.insert flat (declared s) }


addDeclaration :: BoM CompileState m => F.FlatSym -> Declaration -> m ()
addDeclaration flat dec =
    modify $ \s -> s { declarations = Map.insert flat dec (declarations s) }


look :: ModCmp CompileState m => F.FlatSym -> m Object
look flat = do
    ensureDeclared flat
    res <- fmap (Map.lookup flat) (gets objTab)
    case res of
        Just obj -> return obj
        Nothing  -> do
            r <- fmap (catMaybes . map (Map.lookup flat . objTab) . Map.elems) (gets imports)
            case r of
                [] -> error ("no obj for: " ++ flat)
                [x] -> return x


ensureDeclared :: ModCmp CompileState m => F.FlatSym -> m ()
ensureDeclared flat = do
    isDeclared <- fmap (Set.member flat) (gets declared)

    when (not isDeclared) $ do
        res <- fmap (Map.lookup flat) (gets declarations)
        ress <- fmap (map (Map.lookup flat . declarations) . Map.elems) (gets imports)

        case catMaybes (res:ress) of
            []  -> return ()
            [r] -> case r of
                DecType nm                               -> void (typedef nm Nothing)
                DecExtern name paramTypes retType isVarg -> do
                    emitDefn $ GlobalDefinition $ functionDefaults
                        { returnType = retType
                        , name       = name
                        , parameters = ([Parameter typ (mkName "") [] | typ <- paramTypes], isVarg)
                        }

    modify $ \s -> s { declared = Set.insert flat (declared s) }


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
                forM_ (Map.toList $ F.defTab flatState) $ \(flat, obj) ->
                    case obj of
                        F.ObjTypeDef pos typ -> cmpTypeDef flat pos typ
                        _                    -> return ()

                forM_ (Map.toList $ F.defTab flatState) $ \(flat, obj) ->
                    case obj of
                        F.ObjExtern stmt -> cmpExtern stmt
                        _                -> return ()

                forM_ (Map.toList $ F.defTab flatState) $ \(flat, obj) ->
                    case obj of
                        F.ObjFuncDef stmt -> cmpFuncDef stmt
                        _                 -> return ()


cmpTypeDef :: InsCmp CompileState m => F.FlatSym -> TextPos -> T.Type -> m ()
cmpTypeDef flat pos typ = do
    case typ of
        T.I8        -> addObj flat (ObType typ Nothing)
        T.I64       -> addObj flat (ObType typ Nothing)
        T.Bool      -> addObj flat (ObType typ Nothing)
        T.Typedef f -> addObj flat (ObType typ Nothing)
        T.Tuple ts -> do
            name <- freshName (mkBSS flat)
            opTyp <- opTypeOf typ
            typedef name (Just opTyp)
            addDeclared flat
            addDeclaration flat (DecType name)
            addObj flat $ ObType (T.Tuple ts) (Just name)
        _ -> error (show typ)
    return ()


opTypeOf :: ModCmp CompileState m => T.Type -> m Type
opTypeOf typ = case typ of
    T.I64       -> return i64
    T.I32       -> return i32
    T.Bool      -> return i1
    T.Tuple ts  -> fmap (StructureType False) (mapM opTypeOf ts)
    T.Typedef s -> do
        ObType t nm <- look s
        case nm of
            Nothing -> opTypeOf t
            Just n  -> return (NamedTypeReference n)
    _ -> error (show typ) 


cmpExtern :: ModCmp CompileState m => S.Stmt -> m ()
cmpExtern (S.Extern pos sym params retty) = do
    pts  <- forM params $ \(S.Param p s t) -> return t
    pots <- forM params $ \(S.Param p s t) -> opTypeOf t
    rt <- maybe (return VoidType) opTypeOf retty
    addDeclaration sym $ DecExtern (mkName sym) pots rt False
    let op = ConstantOperand $ GlobalReference (ptr $ FunctionType rt pots False) (mkName sym)
    addObj sym (ObjExtern pts retty op)
            


cmpFuncDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpFuncDef (S.Func pos flat params retty blk) = do
    pts  <- forM params $ \(S.Param p s t) -> return t
    pots <- forM params $ \(S.Param p s t) -> opTypeOf t
    pss  <- forM params $ \(S.Param p s t) -> return (mkBSS s)
    rt <- maybe (return VoidType) opTypeOf retty

    name <- freshName (mkBSS flat)
    addDeclaration flat (DecFunc name pots rt)


    let paramNames = map ParameterName pss

    op <- function name (zip pots paramNames) rt $ \a -> do
        return ()

    addObj flat (ObjFunc pts retty op)
    



prettyCompileState :: CompileState -> IO ()
prettyCompileState state = do
    putStrLn "objects:"
    forM_ (Map.toList $ objTab state) $ \(flat, o) ->
        putStrLn $ take 200 (flat ++ ": " ++ show o)
    putStrLn "defs:"
    forM_ (definitions state) $ \d ->
        putStrLn $ take 200 (show d)
