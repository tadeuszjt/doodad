{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Monad where
-- A monad which encapsulates StateT and error handling using Error

import Control.Monad.Fail
import Control.Monad.State 
import Control.Monad.Except 
import Control.Monad.Trans
import Control.Monad.Identity

import Error
import Type
import ASTResolved


newtype DoM s a
    = DoM { getDoM :: StateT s (ExceptT Error IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadError Error)


runDoM :: MonadIO m => s -> DoM s a -> m (Either Error (a, s))
runDoM state dom = do   
    liftIO $ runExceptT $ runStateT (getDoM dom) state


runDoMExcept :: (MonadIO m, MonadError Error m) => s -> DoM s a -> m (a, s)
runDoMExcept state dom = do
    res <- liftIO $ runExceptT $ runStateT (getDoM dom) state
    case res of
        Left e -> throwError e
        Right r -> return r


runDoMUntilSameResult :: Eq a => a -> (a -> DoM s a) -> DoM s (a, Int)
runDoMUntilSameResult a f = runDoMUntilSameResult' 1 a f
    where
        runDoMUntilSameResult' :: Eq a => Int -> a -> (a -> DoM s a) -> DoM s (a, Int)
        runDoMUntilSameResult' n a f = do
            a' <- f a
            if a == a' then return (a, n)
            else runDoMUntilSameResult' (n + 1) a' f



runDoMUntilSameState :: Eq s => DoM s a -> DoM s (a, Int)
runDoMUntilSameState f = runDoMUntilSameState' 1 f
    where
        runDoMUntilSameState' :: Eq s => Int -> DoM s a -> DoM s (a, Int)
        runDoMUntilSameState' n f = do
            s <- get
            a <- f
            s' <- get
            if s == s' then return (a, n)
            else runDoMUntilSameState' (n + 1) f


instance MonadFail (DoM s) where
    fail s = throwError (ErrorStr s)

--instance TypeDefs (DoM ASTResolved) where
--    getTypeDefs = gets typeDefsAll

