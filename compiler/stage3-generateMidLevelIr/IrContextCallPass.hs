{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IrContextCallPass where

import Control.Monad.Except
import Control.Monad.State

import Error
import ASTResolved


data IrContextCallState = IrContextCallState
    {
    }


newtype IrContextCallPass a = IrContextCallPass
    { unIrContextCallPass :: StateT IrContextCallState (StateT ASTResolved (Except Error)) a }
    deriving (Functor, Applicative, Monad, MonadState IrContextCallState, MonadError Error)
