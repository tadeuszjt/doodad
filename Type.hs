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
    | Named String Type
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
        Typedef s     -> s
        Named s t     -> s ++ ":" ++ show t


unNamed (Named s t)   = t
unNamed t             = t

isInt (Named _ t)     = isInt t
isInt x               = (unNamed x) `elem` [I8, I16, I32, I64]

isFloat (Named _ t)   = isFloat t
isFloat x             = (unNamed x) `elem` [F32, F64]

isArray (Named _ t)   = isArray t
isArray (Array _ _)   = True
isArray _             = False

isTuple (Named _ t)   = isTuple t
isTuple (Tuple _)     = True
isTuple _             = False

isTable (Named _ t)   = isTable t
isTable (Table _)     = True
isTable _             = False

isTypedef (Named _ t) = isTypedef t
isTypedef (Typedef _) = True
isTypedef _           = False

isPointer (Named _ t) = isPointer t
isPointer (Pointer _) = True
isPointer _           = False

isIntegral (Named _ t) = isIntegral t
isIntegral x           = isInt x || x == Char

isBase (Named _ t)    = isBase t
isBase x              = isInt x || isFloat x || x == Char || x == Bool

isSimple (Named _ t)  = isSimple t
isSimple x            = isBase x || x == String

isAggregate (Named _ t) = isAggregate t
isAggregate x           = isTuple x || isArray x || isTable x || isPointer x
