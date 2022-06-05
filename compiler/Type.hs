module Type where

import Data.List

data Symbol
    = Sym          { sym :: String }
    | SymQualified { mod :: String, sym :: String }
    | SymResolved  { mod :: String, sym :: String, level :: Int }
    deriving (Eq, Ord)


instance Show Symbol where
    show (Sym s)                     = s
    show (SymQualified mod sym)      = mod ++ "::" ++ sym
    show (SymResolved mod sym level) = mod ++ "::" ++ sym ++ "::" ++ show level


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
    | Tuple [Type]           ----
    | Array Int Type         -- Aggregate Types
    | Table [Type]           --
    | Func [Type] Type       --
    | ADT [Type]
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
        Tuple ts      -> "(" ++ intercalate ", " (map show ts) ++ ")"
        Array n t     -> "[" ++ show n ++ "| " ++ show t ++ "]"
        Table [Char]  -> "string"
        ADT ts        -> "{" ++ intercalate " | " (map show ts) ++ "}"
        Table ts      -> "[" ++ intercalate "; " (map show ts) ++ "]"
        Func ts rt    -> "fn(" ++ intercalate ", " (map show ts) ++ ")" ++ show rt
        Typedef s     -> show s


isInt x                  = x `elem` [I8, I16, I32, I64]

isFloat x                = x `elem` [F32, F64]

isArray (Array _ _)      = True
isArray _                = False

isTuple (Tuple _)        = True
isTuple _                = False

isTable (Table _)        = True
isTable _                = False

isTypedef (Typedef _)    = True
isTypedef _              = False

isFunc (Func _ _)        = True
isFunc _                 = False

isADT (ADT _)            = True
isADT _                  = False

isEmptyADT :: Type -> Bool
isEmptyADT (ADT []) = True
isEmptyADT _        = False


isPtrADT :: Type -> Bool
isPtrADT (ADT [x]) = True
isPtrADT _         = False

isEnumADT :: Type -> Bool
isEnumADT (ADT ts) = all (== Void) ts

isNormalADT :: Type -> Bool
isNormalADT adt@(ADT _) = not $ isEmptyADT adt || isPtrADT adt || isEnumADT adt

isTypeId (Type _)        = True
isTypeId _               = False

isIntegral x             = isInt x || x == Char
isBase x                 = isSimple x || isAggregate x
isSimple x               = isInt x || isFloat x || x == Char || x == Bool
isAggregate x            = isTuple x || isArray x || isTable x || isFunc x


