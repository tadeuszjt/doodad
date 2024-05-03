module SymTab where

import Prelude hiding (lookup, map, filter)
import Control.Monad
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.List as List 


type SymTab s k o = [Map.Map (s, k) o]
initSymTab        = [Map.empty]


lookup :: (Ord s, Ord k) => s -> k -> SymTab s k o -> Maybe o
lookup sym key []     = Nothing
lookup sym key (s:ss) = case Map.lookup (sym, key) s of
    Nothing -> lookup sym key ss
    Just o  -> Just o


lookupWith :: (Ord s, Ord k) => (s -> Bool) -> k -> SymTab s k o -> Maybe s
lookupWith f key [] = Nothing
lookupWith f key (s : ss) = let filteredMap = Map.filterWithKey (\(s, k) o -> f s && k == key) s in
    case Map.keys filteredMap of
        []       -> lookupWith f key ss
        [(s, k)] -> Just s
        ss       -> error "multiple candidates"


lookupAll :: (Ord s, Ord k) => s -> SymTab s k o -> [(k, o)]
lookupAll = error "lookupAll"


lookupHead :: (Ord s, Ord k) => s -> k -> SymTab s k o -> Maybe o
lookupHead sym key []    = Nothing
lookupHead sym key (s:_) = lookup sym key [s]



insert :: (Ord s, Ord k) => s -> k -> o -> SymTab s k o -> SymTab s k o
insert sym key obj (s:ss) =
    (Map.insert (sym, key) obj s):ss


push :: SymTab s k o -> SymTab s k o
push s =
    (Map.empty):s


pop :: SymTab s k o -> SymTab s k o
pop s =
    tail s


prettySymTab :: (Show s, Show k, Show o) => SymTab s k o -> IO ()
prettySymTab symTab = do
    forM_ (reverse symTab) $ \symMap -> do
        putStrLn $ "scope: "
        forM_ (Map.toList symMap) $ \((s, k), o) -> do
                putStrLn $ "\t" ++ show s ++ "\t" ++ show k ++ "\t" ++ show o

        putStrLn ""
