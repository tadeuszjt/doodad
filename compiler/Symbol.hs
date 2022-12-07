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

