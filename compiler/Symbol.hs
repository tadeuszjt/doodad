module Symbol where

data Symbol
    = Sym          { sym :: String }
    | SymQualified { mod :: String, sym :: String }
    | SymResolved  { mod :: String, sym :: String, level :: Int }
    deriving (Eq, Ord)


instance Show Symbol where
    show (Sym s)                     = s
    show (SymQualified mod sym)      = mod ++ "::" ++ sym
    show (SymResolved mod sym level) = case level of
        0 -> mod ++ "_" ++ sym ++ "_0"
        n -> mod ++ "_" ++ sym ++ "_" ++ show n


symbolIsResolved :: Symbol -> Bool
symbolIsResolved (SymResolved _ _ _) = True
symbolIsResolved _                   = False


symbolsCouldMatch :: Symbol -> Symbol -> Bool
symbolsCouldMatch a@(SymResolved _ _ _)  b@(SymResolved _ _ _) = a == b
symbolsCouldMatch a@(SymResolved _ _ _)  b@(SymQualified _ _)  = Symbol.mod a == Symbol.mod b && sym a == sym b
symbolsCouldMatch a@(SymQualified _ _) b@(SymResolved _ _ _)   = Symbol.mod a == Symbol.mod b && sym a == sym b
symbolsCouldMatch a@(SymQualified _ _)   b@(Sym _)             = Symbol.sym a == Symbol.sym b
symbolsCouldMatch a@(Sym _)              b@(SymQualified _ _)  = Symbol.sym a == Symbol.sym b
symbolsCouldMatch a@(SymResolved _ _ _)  b@(Sym _)             = Symbol.sym a == Symbol.sym b
symbolsCouldMatch a@(Sym _)              b@(SymResolved _ _ _) = Symbol.sym a == Symbol.sym b
symbolsCouldMatch a@(Sym _)              b@(Sym _)             = Symbol.sym a == Symbol.sym b
