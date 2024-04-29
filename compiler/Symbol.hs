module Symbol where

import Prelude hiding (mod)


data Symbol
    = Sym          { sym_ :: String }
    | SymQualified { mod_ :: String, sym_ :: String }
    | SymResolved  { sym_ :: String }
    deriving (Eq, Ord)


sym :: Symbol -> String
sym (Sym s) = s
sym (SymQualified m s) = s
sym (SymResolved s)  = takeWhile (/= ':') $ (drop 1 $ dropWhile (/= ':') s)

mod :: Symbol -> String
mod (SymResolved s) = takeWhile (/= ':') s
mod (SymQualified m _) = m


level :: Symbol -> Int
level (SymResolved s)  = read $ reverse $ takeWhile (/= ':') (reverse s)



instance Show Symbol where
    show (Sym s)                = s
    show (SymQualified mod sym) = mod ++ "::" ++ sym
    show s@(SymResolved _) = case level s of
        0 -> mod s ++ "_" ++ sym s
        n -> mod s ++ "_" ++ sym s ++ "_" ++ show (level s)


symbolIsResolved :: Symbol -> Bool
symbolIsResolved (SymResolved _) = True
symbolIsResolved _               = False


symbolsCouldMatch :: Symbol -> Symbol -> Bool
symbolsCouldMatch a@(SymResolved _)    b@(SymResolved _)     = a == b
symbolsCouldMatch a@(SymResolved _)    b@(SymQualified _ _)  = mod a == mod b && sym a == sym b
symbolsCouldMatch a@(SymQualified _ _) b@(SymResolved _)     = mod a == mod b && sym a == sym b
symbolsCouldMatch a@(SymQualified _ _) b@(Sym _)             = sym a == sym b
symbolsCouldMatch a@(Sym _)            b@(SymQualified _ _)  = sym a == sym b
symbolsCouldMatch a@(SymResolved _)    b@(Sym _)             = sym a == sym b
symbolsCouldMatch a@(Sym _)            b@(SymResolved _)     = sym a == sym b
symbolsCouldMatch a@(Sym _)            b@(Sym _)             = sym a == sym b
