module Symbol where

import Prelude hiding (mod)
import Data.List
import Data.Char


data Symbol
    = Sym          { sym_ :: String }
    | SymQualified { mod_ :: String, sym_ :: String }
    | SymResolved  { sym_ :: String, level :: Int }


    | Symbol2         { symStr :: String }
    | Symbol2Resolved { symStr :: String }


    deriving (Eq, Ord, Show)




isResolved :: Symbol -> Bool
isResolved (Symbol2Resolved _) = True
isResolved (Symbol2 _)         = False

isQualified :: Symbol -> Bool
isQualified symbol = length (parseStr (symStr symbol)) > 1



sym :: Symbol -> String
sym (Sym s) = s
sym (SymQualified m s) = s
sym (SymResolved s _)  = reverse $ takeWhile (/= ':') (reverse s)

mod :: Symbol -> String
mod (SymResolved s _) = takeWhile (/= ':') s
mod (SymQualified m _) = m


showSymLocal :: Symbol -> String
showSymLocal symbol@(SymResolved s i) = sym symbol ++ "_" ++ show i
showSymLocal symbol@(Sym s)           = s


showSymGlobal symbol@(SymResolved s i) = mod symbol ++ "_" ++ sym symbol ++ "_" ++ show i


prettySymbol :: Symbol -> String
prettySymbol symbol@(SymResolved s i) = mod symbol ++ "::" ++ sym symbol
prettySymbol symbol@(Sym s)           = s
prettySymbol symbol@(Symbol2 s)       = s
prettySymbol symbol@(Symbol2Resolved s) = s


parseSymbol :: Symbol -> [String]
parseSymbol (SymResolved s i) = parseStr s

parseStr :: String -> [String]
parseStr str = case isInfixOf "::" str of
    True -> takeWhile (/= ':') str : parseStr (drop 2 $ dropWhile (/= ':') str)
    False -> [str]



--
--instance Show Symbol where
--    show symbol = case symbol of
--        Sym s -> s
--        SymQualified mod sym -> mod ++ "::" ++ sym
--        s@(SymResolved _ _) -> case level s of
--            0 -> mod s ++ "_" ++ sym s
--            n -> mod s ++ "_" ++ sym s ++ "_" ++ show (level s)


symbolIsResolved :: Symbol -> Bool
symbolIsResolved (SymResolved _ _) = True
symbolIsResolved _               = False


symbolsCouldMatch :: Symbol -> Symbol -> Bool
symbolsCouldMatch a@(SymResolved _ _)    b@(SymResolved _ _)     = a == b
symbolsCouldMatch a@(SymResolved _ _)    b@(SymQualified _ _)  = mod a == mod b && sym a == sym b
symbolsCouldMatch a@(SymQualified _ _) b@(SymResolved _ _)     = mod a == mod b && sym a == sym b
symbolsCouldMatch a@(SymQualified _ _) b@(Sym _)             = sym a == sym b
symbolsCouldMatch a@(Sym _)            b@(SymQualified _ _)  = sym a == sym b
symbolsCouldMatch a@(SymResolved _ _)    b@(Sym _)             = sym a == sym b
symbolsCouldMatch a@(Sym _)            b@(SymResolved _ _)     = sym a == sym b
symbolsCouldMatch a@(Sym _)            b@(Sym _)             = sym a == sym b
