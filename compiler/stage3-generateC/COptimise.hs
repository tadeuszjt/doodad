{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module COptimise where

import qualified Data.Map as Map
import Control.Monad.State

import Monad
import CAst
import CBuilder


optimise :: BoM BuilderState m => m ()
optimise = do
    elems <- gets elements

    forM_ elems $ \(id, elem) -> case elem of
        Typedef _ _ -> return ()
        Extern _ _ _ -> return ()
        func@(Func _ _ _ _) -> do
            return ()
            _ -> error (show elem)
        
    return ()
