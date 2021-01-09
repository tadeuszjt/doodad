{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Monad where
-- A monad which encapsulates StateT and error handling using CmpError

import Control.Monad.State hiding (fail)
import Control.Monad.Fail
import Control.Monad.Except hiding (void, fail)

import           Control.Monad.Except       hiding (void)
import           Control.Monad.State        hiding (void)
import           Control.Monad.Trans
import           Control.Monad.Identity
import           Control.Monad.Fail
import qualified Data.Set as Set
import qualified Data.Map as Map
import           LLVM.AST 
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import Error


newtype BoMT s m a
    = BoMT { getStateT :: StateT s (ExceptT CmpError m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadError CmpError)

class (MonadState s m, MonadFail m, MonadIO m, MonadError CmpError m) => BoM s m

instance (MonadFail m, MonadIO m) => BoM s (BoMT s m)

instance (Monad m, MonadFail m) => MonadFail (BoMT s m) where
    fail s = throwError $ CmpError (Nothing, s)

instance MonadTrans (BoMT s) where
    lift = BoMT . lift . ExceptT . (fmap Right)


runBoMT :: Monad m => s -> BoMT s m a -> m (Either CmpError (a, s))
runBoMT state bomt =
    runExceptT $ runStateT (getStateT bomt) state


runModuleCmpT
    :: Monad m
    => ModuleBuilderState
    -> s
    -> ModuleCmpT s m a
    -> m (Either CmpError ((a, [Definition]), s))
runModuleCmpT moduleBuilderState state moduleCmpT =
    runBoMT state $ runModuleBuilderT moduleBuilderState (getModuleCmp moduleCmpT)



class (MonadFail m, MonadError CmpError m, BoM s m, MonadModuleBuilder m) => ModCmp s m
class (ModCmp s m, MonadIRBuilder m)                                      => InsCmp s m

instance (Monad m, MonadFail m, MonadIO m) => (ModCmp s) (ModuleCmpT s m)
instance (Monad m, MonadFail m, MonadIO m) => (ModCmp s) (InstrCmpT s m)
instance (Monad m, MonadFail m, MonadIO m) => (InsCmp s) (InstrCmpT s m)

instance MonadTrans (ModuleCmpT s) where
    lift = ModuleCmpT . ModuleBuilderT . lift . lift

instance MonadTrans (InstrCmpT s) where
    lift = InstrCmpT . IRBuilderT . lift . lift

instance (Monad m, MonadFail m, MonadIO m) => MonadFail (InstrCmpT s m) where
    fail s = throwError $ CmpError (Nothing, s)

instance (Monad m, MonadFail m, MonadIO m) => MonadFail (ModuleCmpT s m) where
    fail s = throwError $ CmpError (Nothing, s)


type ModuleCmp s = ModuleCmpT s Identity
newtype ModuleCmpT s m a
    = ModuleCmpT { getModuleCmp :: ModuleBuilderT (BoMT s m) a }
    deriving
        ( Functor, Applicative, Monad, MonadIO, MonadError CmpError, MonadModuleBuilder, BoM s
        , MonadState s)


type InstrCmp s = InstrCmpT s Identity
newtype InstrCmpT s m a
    = InstrCmpT { getInstrCmp :: IRBuilderT (ModuleCmpT s m) a }
    deriving
        ( Functor, Applicative, Monad, MonadIO, MonadError CmpError, MonadModuleBuilder
        , MonadIRBuilder, BoM s, MonadState s)
