{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad where
-- A monad which encapsulates StateT and error handling using CmpError

import Prelude hiding (fail)
import Control.Monad.State hiding (fail)
import Control.Monad.Fail
import Control.Monad.Except hiding (void, fail)

import Error


newtype BoMT s m a
    = BoMT { getStateT :: StateT s (ExceptT CmpError m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadError CmpError)

class (MonadState s m, MonadFail m, MonadIO m) => BoM s m

instance (MonadFail m, MonadIO m) => BoM s (BoMT s m)

instance (Monad m, MonadFail m) => MonadFail (BoMT s m) where
    fail s = throwError $ CmpError (Nothing, s)

runBoMT :: Monad m => s -> BoMT s m a -> m (Either CmpError (a, s))
runBoMT state bomt =
    runExceptT $ runStateT (getStateT bomt) state

