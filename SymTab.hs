module SymTab where

import qualified Data.Map as Map
import           Data.Maybe


type SymTab s k o = [Map.Map s (Map.Map k o)]
initSymTab        = [Map.empty]


lookupSymKey :: (Ord s, Ord k) => s -> k -> SymTab s k o -> Maybe o
lookupSymKey sym key []     = Nothing
lookupSymKey sym key (s:ss) = case Map.lookup sym s of
    Just km -> Map.lookup key km
    Nothing -> lookupSymKey sym key ss


lookupSym :: (Ord s, Ord k) => s -> SymTab s k o -> Maybe (Map.Map k o)
lookupSym sym []     = Nothing
lookupSym sym (s:ss) = case Map.lookup sym s of
    Just km -> Just km
    Nothing -> lookupSym sym ss


lookupHead :: (Ord s, Ord k) => s -> k -> SymTab s k o -> Maybe o
lookupHead sym key []    = Nothing
lookupHead sym key (s:_) = lookupSymKey sym key [s]


insert :: (Ord s, Ord k) => s -> k -> o -> SymTab s k o -> SymTab s k o
insert sym key obj (s:ss) =
    let km = maybe Map.empty id (Map.lookup sym s) in
    (Map.insert sym (Map.insert key obj km) s):ss


push :: SymTab s k o -> SymTab s k o
push s =
    (Map.empty):s


pop :: SymTab s k o -> SymTab s k o
pop s =
    tail s
