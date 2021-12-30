module SymTab where

import Prelude hiding (lookup)
import Control.Monad
import qualified Data.Map as Map
import           Data.Maybe


type SymTab s k o = [Map.Map s (Map.Map k o)]
initSymTab        = [Map.empty]


lookup :: (Ord s, Ord k) => s -> k -> SymTab s k o -> Maybe o
lookup sym key []     = Nothing
lookup sym key (s:ss) = case Map.lookup sym s of
    Just km -> Map.lookup key km
    Nothing -> lookup sym key ss


lookupSym :: (Ord s, Ord k) => s -> SymTab s k o -> Maybe (Map.Map k o)
lookupSym sym []     = Nothing
lookupSym sym (s:ss) = case Map.lookup sym s of
    Just km -> Just km
    Nothing -> lookupSym sym ss


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
            putStrLn $ show s ++ " " ++ show keyMap
        putStrLn ""
