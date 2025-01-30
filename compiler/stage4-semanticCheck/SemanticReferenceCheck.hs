module SemanticReferenceCheck where

import Control.Monad.Except

import Ir
import Error

-- 1.) build up map from id to list of active references
-- 2.) Check no reference are used after invalidation


semanticReferenceCheck :: FuncIr -> Except Error ()
semanticReferenceCheck funcIr = do
    return ()
    
