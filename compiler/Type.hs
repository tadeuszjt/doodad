module Type where

import Data.List

data Symbol
    = Sym { sym :: String }
    | SymQualified { mod :: String, sym :: String }
    deriving (Eq, Ord)


instance Show Symbol where
    show (Sym s)              = s
    show (SymQualified mod s) = mod ++ "::" ++ s


data Type
    = Type Int
    | Void
    | I8                     ----
    | I16                    -- Simple Types
    | I32                    -- 
    | I64                    -- 
    | F32                    --
    | F64                    -- 
    | Bool                   --
    | Char                   --
    | Tuple [(String, Type)] ----
    | Array Int Type         -- Aggregate Types
    | Table [Type]           --
    | ADT [(String, Type)]   --
    | Func [Type] Type 
    | Typedef Symbol
    deriving (Eq, Ord)


instance Show Type where
    show t = case t of
        Type id       -> "t" ++ show id
        Void          -> "void"
        I8            -> "i8"
        I16           -> "i16"
        I32           -> "i32"
        I64           -> "i64"
        F32           -> "f32"
        F64           -> "f64"
        Bool          -> "bool"
        Char          -> "char"
        Tuple xs      -> "(" ++ intercalate ", " (map showTupArg xs) ++ ")"
        Array n t     -> "[" ++ show n ++ "| " ++ show t ++ "]"
        Table [Char]  -> "string"
        Table ts      -> "[" ++ intercalate "; " (map show ts) ++ "]"
        ADT ts        -> "{" ++ intercalate ", " (map show ts) ++ "}"
        Typedef s     -> show s
        Func ts rt    -> "fn(" ++ intercalate ", " (map show ts) ++ ")" ++ show rt
        where
            showTupArg :: (String, Type) -> String
            showTupArg ("", t) = show t
            showTupArg (s, t)  = s ++ ":" ++ show t


isInt x                    = x `elem` [I8, I16, I32, I64]

isFloat x                  = x `elem` [F32, F64]

isArray (Array _ _)        = True
isArray _                  = False

isTuple (Tuple _)          = True
isTuple _                  = False

isTable (Table _)          = True
isTable _                  = False

isTypedef (Typedef _)      = True
isTypedef _                = False

isADT (ADT _)              = True
isADT _                    = False

isEmptyADT typ             = typ == ADT []

isPtrADT (ADT [(_, Void)]) = False
isPtrADT (ADT [_])         = True
isPtrADT _                 = False

isEnumADT (ADT [])         = False
isEnumADT (ADT xs)         = all (== Void) $ map snd xs
isEnumADT _                = False

isNormalADT typ@(ADT xs)   = not (isEmptyADT typ || isPtrADT typ || isEnumADT typ)
isNormalADT _              = False

isIntegral x               = isInt x || x == Char

isBase x                   = isInt x || isFloat x || x == Char || x == Bool

isSimple x                 = isBase x

isAggregate x              = isTuple x || isArray x || isTable x || isADT x

isFunction (Func _ _)      = True
isFunction _               = False


