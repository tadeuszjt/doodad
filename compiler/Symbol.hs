module Symbol where

import Prelude hiding (mod)
import Data.List
import Data.Char


data Symbol
    = Sym          { symStr :: [String] }
    | SymResolved  { symStr :: [String] }
    | SymCandidates [Symbol]
    deriving (Eq, Ord, Show)


sym :: Symbol -> String
sym (Sym s)          = last s
sym (SymResolved s ) = last $ dropLevels s


dropLevels :: [String] -> [String]
dropLevels = filter (\s -> not $ isDigit $ head s)


symbolModule :: Symbol -> String
symbolModule (SymResolved s) = head s


showSymLocal :: Symbol -> String
showSymLocal symbol@(SymResolved s) = intercalate "_" (tail s)

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

