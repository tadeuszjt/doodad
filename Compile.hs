{-# LANGUAGE FlexibleContexts #-}
module Compile where

import Monad
import Error
import qualified AST as S
import qualified SymTab
import qualified Modules as M


data SymKey
    = SymKey
    deriving (Show, Eq)


data SymObj
    = SymObj

data CmpState
    = CmpState
        { symTab :: SymTab.SymTab SymKey SymObj
        }

cmpTypeDefs :: BoM CmpState m => [S.Stmt] -> m ()
cmpTypeDefs = undefined
