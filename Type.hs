module Type where

import Data.Word
import Data.List

data Type
    = Void
    | I8                     ----
    | I16                    -- Simple Types
    | I32                    -- 
    | I64                    -- 
    | F32                    --
    | F64                    -- 
    | Bool                   --
    | Char                   --
    | String                 ----
    | Tuple [Type]
    | Array Word Type
    | Table [Type]
    | Pointer [Type]
    | Typedef String
    deriving (Eq, Ord)


instance Show Type where
    show t = case t of
        Void          -> "void"
        I8            -> "i8"
        I16           -> "i16"
        I32           -> "i32"
        I64           -> "i64"
        F32           -> "f32"
        F64           -> "f64"
        Bool          -> "bool"
        Char          -> "char"
        String        -> "string"
        Tuple ts      -> "(" ++ intercalate ", " (map show ts) ++ ")"
        Array n t     -> "[" ++ show n ++ "| " ++ show t ++ "]"
        Table ts      -> "[" ++ intercalate "; " (map show ts) ++ "]"
        Pointer ts    -> "{" ++ intercalate ", " (map show ts) ++ "}"
        Typedef s     -> show s


isChar x              = x == Char

isString x            = x == String

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

isPointer (Pointer _) = True
isPointer _           = False

isIntegral x          = isInt x || isChar x

isBase x              = isInt x || isFloat x || isChar x || x == Bool

isSimple x            = isBase x || isString x

isAggregate x         = isTuple x || isArray x || isTable x
