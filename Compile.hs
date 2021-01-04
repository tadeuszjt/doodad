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
import Flatten as F

mkBSS = BSS.toShort . BS.pack


data Obj
    = ObType T.Type
    deriving (Show)


data Declaration
    = DecType Name 
    deriving (Show)


data CompileState
    = CompileState
        { declarations :: Map.Map FlatSym Declaration
        , definitions  :: [Definition]
        , declared     :: Set.Set FlatSym
        , tab          :: Map.Map FlatSym Obj
        }
    deriving (Show)

initCompileState
     = CompileState
        { declarations = Map.empty
        , definitions  = []
        , declared     = Set.empty
        , tab          = Map.empty
        }



addObj :: BoM CompileState m => FlatSym -> Obj -> m ()
addObj flat obj =
    modify $ \s -> s { tab = Map.insert flat obj (tab s) }


addDeclared :: BoM CompileState m => FlatSym -> m ()
addDeclared flat =
    modify $ \s -> s { declared = Set.insert flat (declared s) }


addDeclaration :: BoM CompileState m => FlatSym -> Declaration -> m ()
addDeclaration flat dec =
    modify $ \s -> s { declarations = Map.insert flat dec (declarations s) }


compileFlatState
    :: (Monad m, MonadFail m, MonadIO m)
    => Map.Map S.ModuleName CompileState
    -> FlattenState
    -> m (Either CmpError CompileState)
compileFlatState importCompiled flatState = do
    res <- runModuleCmpT emptyModuleBuilder initCompileState f
    case res of
        Left err                  -> return (Left err)
        Right (((), defs), state) -> return (Right state { definitions = defs })
    where
        ensureDeclared :: ModCmp CompileState m => FlatSym -> m ()
        ensureDeclared flat = do
            isDeclared <- fmap (Set.member flat) (gets declared)

            when (not isDeclared) $ do
                res <- fmap (Map.lookup flat) (gets declarations)
                let ress = map (Map.lookup flat . declarations) (Map.elems importCompiled)

                case catMaybes (res:ress) of
                    []  -> return ()
                    [r] -> case r of
                        DecType nm -> void (typedef nm Nothing)

            modify $ \s -> s { declared = Set.insert flat (declared s) }


        look :: ModCmp CompileState m => FlatSym -> m Obj
        look flat = do
            ensureDeclared flat
            res <- fmap (Map.lookup flat) (gets tab)
            case res of
                Just obj -> return obj
                Nothing  -> return $ (head . catMaybes) $ map (Map.lookup flat . tab) (Map.elems importCompiled)


        f :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        f = void $ function "main" [] VoidType $ \_ ->
                getInstrCmp cmp

        cmp :: InsCmp CompileState m => m ()
        cmp = do
            forM_ (Map.toList $ defTab flatState) $ \(flat, obj) ->
                case obj of
                    ObjTypeDef pos typ -> cmpTypeDef flat pos typ
                    _ ->               return ()


        cmpTypeDef :: ModCmp CompileState m => FlatSym -> TextPos -> T.Type -> m ()
        cmpTypeDef flat pos typ = do
            case typ of
                T.I8        -> addObj flat (ObType T.I8)
                T.I64       -> addObj flat (ObType T.I64)
                T.Bool      -> addObj flat (ObType T.Bool)
                T.Typedef f -> addObj flat (ObType (T.Typedef f))
                T.Tuple ts -> do
                    let name = Name (mkBSS flat)
                    opTyp <- opTypeOf typ
                    typedef name (Just opTyp)
                    addDeclared flat
                    addDeclaration flat (DecType name)
                    addObj flat (ObType (T.Tuple ts))
                _ -> error (show typ)
            return ()


        opTypeOf :: ModCmp CompileState m => T.Type -> m Type
        opTypeOf typ = case typ of
            T.I64       -> return i64
            T.I32       -> return i32
            T.Tuple ts  -> fmap (StructureType False) (mapM opTypeOf ts)
            T.Typedef s -> do ObType t <- look s; opTypeOf t



prettyCompileState :: CompileState -> IO ()
prettyCompileState state = do
    putStrLn "objects:"
    forM_ (Map.toList $ tab state) $ \(flat, o) ->
        putStrLn $ take 100 (flat ++ ": " ++ show o)
    putStrLn "defs:"
    forM_ (definitions state) $ \d ->
        putStrLn $ take 100 (show d)
