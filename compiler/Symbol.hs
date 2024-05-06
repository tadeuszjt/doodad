module Symbol where

import Prelude hiding (mod)
import Data.List
import Data.Char


data Symbol
    = Sym          { symStr :: [String] }
    | SymResolved  { symStr :: [String] }
    deriving (Eq, Ord, Show)


sym :: Symbol -> String
sym (Sym s)          = last s
sym (SymResolved s ) = last $ dropLevels s


dropLevels :: [String] -> [String]
dropLevels = filter (\s -> not $ isDigit $ head s)


parseStr_ :: String -> [String]
parseStr_ []  = []
parseStr_ str = let (pre, post) = span (/= ':') str in
    pre : parseStr_ (dropWhile (==':') post)

mkSym :: String -> Symbol
mkSym str = Sym (parseStr_ str)



mod :: Symbol -> String
mod (SymResolved s) = head s


showSymLocal :: Symbol -> String
showSymLocal symbol@(SymResolved s) = intercalate "_" (s)
showSymLocal symbol@(Sym s)         = intercalate "_" s


showSymGlobal symbol@(SymResolved s) = intercalate "_" (s)


prettySymbol :: Symbol -> String
prettySymbol symbol@(SymResolved s)  = intercalate "::" (s)
prettySymbol symbol@(Sym s)          = intercalate "::" s


symbolIsResolved :: Symbol -> Bool
symbolIsResolved (SymResolved _) = True
symbolIsResolved _               = False


symbolsCouldMatch :: Symbol -> Symbol -> Bool
symbolsCouldMatch (SymResolved a)  (SymResolved b) = a == b
symbolsCouldMatch (Sym a)          (SymResolved b) = isSuffixOf (a) (dropLevels $ b)
symbolsCouldMatch (SymResolved b)  (Sym a)         = isSuffixOf (a) (dropLevels $ b)
symbolsCouldMatch (Sym a)          (Sym b)         = a == b

