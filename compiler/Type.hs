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
    | Func [Type] Type 
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
        Func ts rt    -> "fn(" ++ intercalate ", " (map show ts) ++ ")" ++ show rt


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


