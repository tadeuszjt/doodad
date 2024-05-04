module Symbol where

import Prelude hiding (mod)
import Data.List
import Data.Char


data Symbol
    = Sym         { symStr :: String }
    | SymResolved { symStr :: String }


    deriving (Eq, Ord, Show)




isResolved :: Symbol -> Bool
isResolved (SymResolved _) = True
isResolved (Sym _)         = False

isQualified :: Symbol -> Bool
isQualified symbol = length (parseStr (symStr symbol)) > 1

--showSymLocal :: Symbol -> String
--showSymLocal symbol@(SymResolved s i) = sym symbol ++ "_" ++ show i
--showSymLocal symbol@(Sym s)           = s


--showSymGlobal symbol@(SymResolved s i) = mod symbol ++ "_" ++ sym symbol ++ "_" ++ show i


prettySymbol :: Symbol -> String
prettySymbol symbol@(Sym s)       = s
prettySymbol symbol@(SymResolved s) = s


parseStr :: String -> [String]
parseStr str = case isInfixOf "::" str of
    True -> takeWhile (/= ':') str : parseStr (drop 2 $ dropWhile (/= ':') str)
    False -> [str]
