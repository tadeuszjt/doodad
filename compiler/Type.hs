module Type where

import Data.List

data Type
    = Self
    | Void
    | I8                     ----
    | I16                    -- Simple Types
    | I32                    -- 
    | I64                    -- 
    | F32                    --
    | F64                    -- 
    | Bool                   --
    | Char                   --
    | Tuple [(String, Type)]
    | Array Int Type
    | Table [Type]
    | ADT [(String, Type)]
    | Typedef String
    deriving (Eq, Ord)


instance Show Type where
    show t = case t of
        Self          -> "self"
        Void          -> "void"
        I8            -> "i8"
        I16           -> "i16"
        I32           -> "i32"
        I64           -> "i64"
        F32           -> "f32"
        F64           -> "f64"
        Bool          -> "bool"
        Char          -> "char"
        Tuple ts      -> "(" ++ intercalate ", " (map show ts) ++ ")"
        Array n t     -> "[" ++ show n ++ "| " ++ show t ++ "]"
        Table ts      -> "[" ++ intercalate "; " (map show ts) ++ "]"
        ADT ts        -> "{" ++ intercalate ", " (map show ts) ++ "}"
        Typedef s     -> s


isInt x               = x `elem` [I8, I16, I32, I64]

isFloat x             = x `elem` [F32, F64]

isArray (Array _ _)   = True
isArray _             = False

isTuple (Tuple _)     = True
isTuple _             = False

isTable (Table _)     = True
isTable _             = False

isTypedef (Typedef _) = True
isTypedef _           = False

isADT (ADT _)         = True
isADT _               = False

isIntegral x  = isInt x || x == Char

isBase x      = isInt x || isFloat x || x == Char || x == Bool

isSimple x    = isBase x

isAggregate x = isTuple x || isArray x || isTable x || isADT x
