module SymTab where

import Prelude hiding (lookup, map, filter)
import Control.Monad
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.List as List 


type SymTab s o = [Map.Map s o]
initSymTab      = [Map.empty]


lookup :: (Ord s) => s -> SymTab s o -> Maybe o
lookup sym []     = Nothing
lookup sym (s:ss) = case Map.lookup sym s of
    Just o -> Just o
    Nothing -> lookup sym ss


lookupAll :: (Ord s) => s -> SymTab s o -> [o]
lookupAll sym [] = []
lookupAll sym (s : ss) = case lookup sym [s] of
    Nothing -> lookupAll sym ss
    Just o  -> o : lookupAll sym ss


lookupHead :: (Ord s) => s -> SymTab s o -> Maybe o
lookupHead sym []    = Nothing
lookupHead sym (s:_) = lookup sym [s]


insert :: (Ord s) => s -> o -> SymTab s o -> SymTab s o
insert sym obj (s:ss) =
    (Map.insert sym obj s):ss


push :: SymTab s o -> SymTab s o
push s =
    (Map.empty):s


pop :: SymTab s o -> SymTab s o
pop s =
    tail s


prettySymTab :: (Show s, Show o) => SymTab s o -> IO ()
prettySymTab symTab = do
    forM_ (reverse symTab) $ \symMap -> do
        putStrLn $ "scope: "
        forM_ (Map.toList symMap) $ \(s, v) -> do
                putStrLn $ "\t" ++ show s ++ "\t" ++ show v

        putStrLn ""
