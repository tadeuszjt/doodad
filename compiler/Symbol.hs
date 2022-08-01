module Symbol where

import qualified Data.Map as Map
import qualified SymTab 

data Symbol
    = Sym          { sym :: String }
    | SymQualified { mod :: String, sym :: String }
    | SymResolved  { mod :: String, sym :: String, level :: Int }
    deriving (Eq, Ord)


instance Show Symbol where
    show (Sym s)                     = s
    show (SymQualified mod sym)      = mod ++ "::" ++ sym
    show (SymResolved mod sym level) = mod ++ "." ++ sym ++ "." ++ show level


lookupSym
    :: Ord k
    => Symbol
    -> SymTab.SymTab Symbol k o
    -> [SymTab.SymTab Symbol k o]
    -> [(k, o)]
lookupSym symbol symTab imports =
    SymTab.lookupSym symbol symTab ++ (concat $ map (SymTab.lookupSym symbol) imports)

