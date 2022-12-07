module SymTab where

import Prelude hiding (lookup, map, filter)
import Control.Monad
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.List as List 


type SymTab s k o = [Map.Map s (Map.Map k o)]
initSymTab        = [Map.empty]


lookup :: (Ord s, Ord k) => s -> k -> SymTab s k o -> Maybe o
lookup sym key []     = Nothing
lookup sym key (s:ss) = case Map.lookup sym s of
    Just km -> Map.lookup key km
    Nothing -> lookup sym key ss


lookupSym :: (Ord s, Ord k) => s -> SymTab s k o -> [(k, o)]
lookupSym sym []     = []
lookupSym sym (s:ss) = case Map.lookup sym s of
    Just km -> Map.toList km
    Nothing -> lookupSym sym ss


lookupKey :: (Ord s, Ord k) => k -> SymTab s k o -> [(s, o)]
lookupKey key [] = []
lookupKey key (s:ss) =
    Map.toList (Map.mapMaybe (Map.lookup key) s) ++ lookupKey key ss


lookupHead :: (Ord s, Ord k) => s -> k -> SymTab s k o -> Maybe o
lookupHead sym key []    = Nothing
lookupHead sym key (s:_) = lookup sym key [s]


deleteHead :: (Ord s, Ord k) => s -> k -> SymTab s k o -> SymTab s k o
deleteHead sym key [] = []
deleteHead sym key (s:ss)
    | isNothing (Map.lookup sym s)             = (s:ss)
    | isNothing (Map.lookup key $ s Map.! sym) = (s:ss)
    | Map.size (s Map.! sym) == 1              = (Map.delete sym s:ss)
    | otherwise                                = (Map.adjust (Map.delete key) sym s:ss)


map :: (o1 -> o2) -> SymTab s k o1 -> SymTab s k o2
map f []     = []
map f (s:ss) = (Map.map (Map.map f) s:map f ss)


mapKeys :: (Ord k2) => (k1 -> k2) -> SymTab s k1 o -> SymTab s k2 o
mapKeys f [] = []
mapKeys f (s:ss) = Map.map (Map.mapKeys f) s : mapKeys f ss


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


prettySymTab :: (Show s, Show k, Show o) => SymTab s k o -> IO ()
prettySymTab symTab = do
    forM_ (reverse symTab) $ \symMap -> do
        forM_ (Map.toList symMap) $ \(s, keyMap) -> do
            forM_ (Map.toList keyMap) $ \(k, v) -> do
                putStrLn $ show s ++ "\t" ++ show k ++ "\t" ++ show v

        putStrLn ""
