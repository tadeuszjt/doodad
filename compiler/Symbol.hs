module Symbol where

import Prelude hiding (mod)
import Data.List
import Data.Char


data Symbol
    = Sym          { symStr :: String }
    | SymResolved  { symStr :: String }
    deriving (Eq, Ord, Show)


sym :: Symbol -> String
sym (Sym s)          = last (parseStr s)
sym (SymResolved s ) = last $ dropLevels (parseStr s)


dropLevels :: [String] -> [String]
dropLevels = filter (\s -> not $ isDigit $ head s)


parseStr :: String -> [String]
parseStr str = case isInfixOf "::" str of
    True -> takeWhile (/= ':') str : parseStr (dropWhile (==':') $ dropWhile (/= ':') str)
    False -> [str]


mod :: Symbol -> String
mod (SymResolved s) = head (parseStr s)


showSymLocal :: Symbol -> String
showSymLocal symbol@(SymResolved s) = intercalate "_" (parseStr s)
showSymLocal symbol@(Sym s)           = s


showSymGlobal symbol@(SymResolved s) = intercalate "_" (parseStr s)


prettySymbol :: Symbol -> String
prettySymbol symbol@(SymResolved s)  = intercalate "::" (parseStr s)
prettySymbol symbol@(Sym s)          = s


symbolIsResolved :: Symbol -> Bool
symbolIsResolved (SymResolved _) = True
symbolIsResolved _               = False


symbolsCouldMatch :: Symbol -> Symbol -> Bool
symbolsCouldMatch (SymResolved a)  (SymResolved b) = a == b
symbolsCouldMatch (Sym a)          (SymResolved b) = isPrefixOf (reverse $ parseStr a) (reverse $ dropLevels $ parseStr b)
symbolsCouldMatch (SymResolved b)  (Sym a)         = isPrefixOf (reverse $ parseStr a) (reverse $ dropLevels $ parseStr b)
symbolsCouldMatch (Sym a)          (Sym b)         = a == b

