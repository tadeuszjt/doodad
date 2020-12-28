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
import qualified Modules as M
import qualified Flatten as F

mkBSS = BSS.toShort . BS.pack


data CompileState
    = CompileState
        { actions :: Map.Map F.FlatSym (ModuleCmpT CompileState Identity ())
        , defined :: Set.Set F.FlatSym
        }

initCompileState
     = CompileState
        { actions = Map.empty
        , defined = Set.empty
        }


addAction :: BoM CompileState m => F.FlatSym -> (ModuleCmpT CompileState Identity ()) -> m ()
addAction flat f = do
    res <- fmap (Map.lookup flat) (gets actions)
    when (isJust res) $ fail (flat ++ ": action already created")
    modify $ \s -> s { actions = Map.insert flat f (actions s) }


compileFlatState :: (Monad m, MonadFail m, MonadIO m) => F.FlattenState -> m (Either CmpError CompileState)
compileFlatState flatState = do
    res <- runBoMT initCompileState (runModuleCmpT emptyModuleBuilder initCompileState f)
    case res of
        Left err         -> return (Left err)
        Right (_, state) -> return (Right state) 
    where
        f :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        f =
            void $ function "main" [] VoidType $ \_ -> do
                getInstrCmp cmp

        cmp :: InsCmp CompileState m => m ()
        cmp = do
            forM_ (Map.toList $ F.typedefs flatState) $ \(flat, (pos, typ)) ->
                cmpTypeDef flat pos typ


cmpTypeDef :: InsCmp CompileState m => F.FlatSym -> TextPos -> T.Type -> m ()
cmpTypeDef flat pos typ = do
    name <- freshName (mkBSS flat)
    addAction flat (void $ typedef name Nothing)



prettyCompileState :: CompileState -> IO ()
prettyCompileState state = do
    putStrLn "actions:"
    forM_ (Map.toList $ actions state) $ \(flat, mod) -> putStrLn (flat ++ " modcmp")
