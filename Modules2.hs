module Modules2 where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S


data Module
    = Module
        { filepaths :: Set.Set String
        , result    :: Maybe ()
        }


data Modules
    = Modules
        { modMap :: Map.Map String Module
        }
