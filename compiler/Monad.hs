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


runBoMUntilSameResult :: Eq a => BoM s m => a -> (a -> m a) -> m a
runBoMUntilSameResult a f = do
    a' <- f a
    if a == a' then return a
    else runBoMUntilSameResult a' f

runBoMUntilSameState :: Eq s => BoM s m => m a -> m (a, Int)
runBoMUntilSameState f = runBoMUntilSameState' 1 f
    where
        runBoMUntilSameState' :: Eq s => BoM s m => Int -> m a -> m (a, Int)
        runBoMUntilSameState' n f = do
            s <- get
            a <- f
            s' <- get
            if s == s' then return (a, n)
            else runBoMUntilSameState' (n + 1) f


class (MonadState s m, MonadFail m, MonadIO m, MonadError Error m) => BoM s m

instance (MonadFail m, MonadIO m) => BoM s (BoMT s m)

instance (Monad m, MonadFail m) => MonadFail (BoMT s m) where
    fail s = throwError (ErrorStr s)

instance MonadTrans (BoMT s) where
    lift = BoMT . lift . ExceptT . (fmap Right)
