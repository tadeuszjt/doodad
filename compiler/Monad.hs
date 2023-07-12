{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Monad where
-- A monad which encapsulates StateT and error handling using Error

import Control.Monad.Fail
import Control.Monad.State 
import Control.Monad.Except 
import Control.Monad.Trans
import Control.Monad.Identity


import Error


newtype BoMT s m a
    = BoMT { getStateT :: StateT s (ExceptT Error m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadError Error)


runBoMT :: Monad m => s -> BoMT s m a -> m (Either Error (a, s))
runBoMT state bomt =
    runExceptT $ runStateT (getStateT bomt) state


runBoMTExcept :: BoM s m => s1 -> BoMT s1 m a -> m (a, s1)
runBoMTExcept state bomt = do
    res <- runExceptT $ runStateT (getStateT bomt) state
    case res of
        Left e -> throwError e
        Right r -> return r


class (MonadState s m, MonadFail m, MonadIO m, MonadError Error m)     => BoM s m


instance (MonadFail m, MonadIO m) => BoM s (BoMT s m)

instance (Monad m, MonadFail m) => MonadFail (BoMT s m) where
    fail s = throwError (ErrorStr s)

instance MonadTrans (BoMT s) where
    lift = BoMT . lift . ExceptT . (fmap Right)
