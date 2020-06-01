module SymTab where

import qualified Data.Map as Map


type SymTab a b = [Map.Map a b]
initSymTab      = [Map.empty]


lookup :: Ord a => a -> SymTab a b -> Maybe b
lookup sym []     = Nothing
lookup sym (s:ss) = case Map.lookup sym s of
	Just x  -> Just x
	Nothing -> SymTab.lookup sym ss


insert :: Ord a => a -> b -> SymTab a b -> SymTab a b
insert sym item (s:ss) =
	(Map.insert sym item s):ss


push :: SymTab a b -> SymTab a b
push s =
	(Map.empty):s


pop :: SymTab a b -> SymTab a b
pop s =
	tail s
