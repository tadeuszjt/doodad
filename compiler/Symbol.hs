module Symbol where

import qualified Data.Map as Map
import Data.Maybe
import qualified SymTab 

data Symbol
    = Sym          { sym :: String }
    | SymQualified { mod :: String, sym :: String }
    | SymResolved  { mod :: String, sym :: String, level :: Int }
    deriving (Eq, Ord)


instance Show Symbol where
    show (Sym s)                     = s
    show (SymQualified mod sym)      = mod ++ "::" ++ sym
    show (SymResolved mod sym level) = case level of
        0 -> "_" ++ sym
        n -> "_" ++ sym ++ "_" ++ show n


lookupSym
    :: Ord k
    => Symbol
    -> SymTab.SymTab Symbol k o
    -> [SymTab.SymTab Symbol k o]
    -> [(k, o)]
lookupSym symbol symTab imports =
    SymTab.lookupSym symbol symTab ++ (concat $ map (SymTab.lookupSym symbol) imports)


lookupSymKey
    :: (Ord k, Ord s)
    => s
    -> k
    -> SymTab.SymTab s k o
    -> [SymTab.SymTab s k o]
    -> Maybe o
lookupSymKey symbol key symTab imports =
    case SymTab.lookup symbol key symTab of
        Just o -> Just o
        Nothing -> case catMaybes (map (SymTab.lookup symbol key) imports) of
            [] -> Nothing
            [o] -> Just o
            _ -> error "symbol ambiguous"

