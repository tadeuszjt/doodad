module Type where

import Data.Word
import Data.List
import LLVM.AST  hiding (function, Module)


data ValType
    = Void
    | I8
    | I16
    | I32
    | I64
    | F32
    | F64
    | Bool
    | Char
    | String
    | Tuple (Maybe Name) [ValType]
    | Array Word ValType
    | Table (Maybe Name) [ValType]
    | Typedef String
    | Annotated String ValType
    deriving (Eq, Ord)


instance Show ValType where
    show t = case t of
        Void        -> "void"
        I8          -> "i8"
        I16         -> "i16"
        I32         -> "i32"
        I64         -> "i64"
        F32         -> "f32"
        F64         -> "f64"
        Bool        -> "bool"
        Char        -> "char"
        String      -> "string"
        Tuple nm ts -> "(" ++ intercalate ", " (map show ts) ++ ")"
        Array n t   -> "[" ++ show n ++ " " ++ show t ++ "]"
        Table nm ts -> "{" ++ intercalate "; " (map show ts) ++ "}"
        Typedef s   -> s
        Annotated _ t -> show t


isChar (Annotated _ t)      = isChar t
isChar x                    = x == Char
isString (Annotated _ t)    = isString t
isString x                  = x == String
isInt (Annotated _ t)       = isInt t
isInt x                     = x `elem` [I8, I16, I32, I64]
isFloat (Annotated _ t)     = isFloat t
isFloat x                   = x `elem` [F32, F64]
isArray (Annotated _ t)     = isArray t
isArray (Array _ _)         = True
isArray _                   = False
isTuple (Annotated _ t)     = isTuple t
isTuple (Tuple _ _)         = True
isTuple _                   = False
isTable (Annotated _ t)     = isTable t
isTable (Table _ _)         = True
isTable _                   = False
isTypedef (Typedef _)       = True
isTypedef _                 = False
isAnnotated (Annotated _ _) = True
isAnnotated _               = False
isIntegral x                = isInt x || isChar x
isBase x                    = isInt x || isFloat x || isChar x
isSimple x                  = isBase x || isString x
isAggregate x               = isTuple x || isArray x || isTable x