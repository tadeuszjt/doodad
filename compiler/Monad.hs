{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Monad where
-- A monad which encapsulates StateT and error handling using Error

import Control.Monad.State 
import Control.Monad.Except 
import Control.Monad.Trans
import Control.Monad.Identity

import LLVM.AST 
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import Error


newtype BoMT s m a
    = BoMT { getStateT :: StateT s (ExceptT Error m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadError Error)


type ModuleCmp s = ModuleCmpT s Identity
newtype ModuleCmpT s m a
    = ModuleCmpT { getModuleCmp :: ModuleBuilderT (BoMT s m) a }
    deriving
        ( Functor, Applicative, Monad, MonadIO, MonadError Error, MonadModuleBuilder, BoM s
        , MonadState s)


type InstrCmp s = InstrCmpT s Identity
newtype InstrCmpT s m a
    = InstrCmpT { getInstrCmp :: IRBuilderT (ModuleCmpT s m) a }
    deriving
        ( Functor, Applicative, Monad, MonadIO, MonadError Error, MonadModuleBuilder
        , MonadIRBuilder, BoM s, MonadState s)


runAll :: Monad m => s -> InstrCmpT s m a -> m (Either Error (a, [BasicBlock], [Definition], s))
runAll initState instrCmpT = do
    res <- runBoMT initState $ runModuleCmpT emptyModuleBuilder $ runInstrCmpT emptyIRBuilder instrCmpT
    case res of
        Left e                             -> return $ Left e
        Right (((a, blocks), defs), state) -> return $ Right (a, blocks, defs, state)


runBoMT :: Monad m => s -> BoMT s m a -> m (Either Error (a, s))
runBoMT state bomt =
    runExceptT $ runStateT (getStateT bomt) state


runModuleCmpT :: Monad m => ModuleBuilderState -> ModuleCmpT s m a -> BoMT s m (a, [Definition])
runModuleCmpT moduleBuilderState moduleCmpT =
    runModuleBuilderT moduleBuilderState (getModuleCmp moduleCmpT)


runInstrCmpT :: Monad m => IRBuilderState -> InstrCmpT s m a -> ModuleCmpT s m (a, [BasicBlock])
runInstrCmpT irBuilderState instrCmpT =
    runIRBuilderT irBuilderState (getInstrCmp instrCmpT)


class (MonadState s m, MonadFail m, MonadIO m, MonadError Error m)     => BoM s m
class (MonadFail m, MonadError Error m, BoM s m, MonadModuleBuilder m) => ModCmp s m
class (ModCmp s m, MonadIRBuilder m)                                   => InsCmp s m


instance (MonadFail m, MonadIO m) => BoM s (BoMT s m)

instance (Monad m, MonadFail m) => MonadFail (BoMT s m) where
    fail s = throwError (ErrorStr s)

instance MonadTrans (BoMT s) where
    lift = BoMT . lift . ExceptT . (fmap Right)

instance (Monad m, MonadFail m, MonadIO m) => (ModCmp s) (ModuleCmpT s m)
instance (Monad m, MonadFail m, MonadIO m) => (ModCmp s) (InstrCmpT s m)
instance (Monad m, MonadFail m, MonadIO m) => (InsCmp s) (InstrCmpT s m)

instance MonadTrans (ModuleCmpT s) where
    lift = ModuleCmpT . ModuleBuilderT . lift . lift

instance MonadTrans (InstrCmpT s) where
    lift = InstrCmpT . IRBuilderT . lift . lift

instance (Monad m, MonadFail m, MonadIO m) => MonadFail (InstrCmpT s m) where
    fail s = throwError $ (ErrorStr s)

instance (Monad m, MonadFail m, MonadIO m) => MonadFail (ModuleCmpT s m) where
    fail s = throwError $ (ErrorStr s)

