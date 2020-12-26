{-# LANGUAGE FlexibleContexts #-}
module Compile where

import Monad
import Error
import qualified AST as S
import qualified SymTab
import qualified Modules as M
import qualified Data.Map as Map


data CmpState
    = CmpState
        { 
        }

cmpTypeDefs :: BoM CmpState m => [S.Stmt] -> m ()
cmpTypeDefs = undefined
