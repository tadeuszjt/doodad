module Type where

import Data.List
import Symbol

data AdtField
    = FieldNull
    | FieldType Type
    | FieldCtor [Type]
    deriving (Eq, Ord)

data Type
    = Type Int
    | Void
    | I8                     
    | I16                    
    | I32                    
    | I64                    
    | F32                    
    | F64                    
    | Bool                   
    | Char                   
    | Enum
    | Range Type
    | Sparse [Type]
    | Tuple [Type]           
    | Array Int Type         
    | Table [Type]         
    | Func [Type] Type 
    | ADT [AdtField]
    | Typedef Symbol
    | UnsafePtr
    deriving (Eq, Ord)


instance Show AdtField where
    show adtField = case adtField of
        FieldNull -> "null"
        FieldCtor ts -> "(" ++ intercalate ", " (map show ts) ++ ")"
        FieldType t -> show t

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
        Enum          -> "enum"
        Range t       -> "[..]" ++ show t
        Sparse ts     -> "sparse" ++ "[" ++ intercalate "; " (map show ts) ++ "]"
        Tuple ts      -> "(" ++ intercalate ", " (map show ts) ++ ")"
        Array n t     -> "[" ++ show n ++ " " ++ show t ++ "]"
        ADT tss       -> "{" ++ intercalate " | " (map show tss) ++ "}"
        Table ts      -> "[" ++ intercalate "; " (map show ts) ++ "]"
        Func ts rt    -> "fn(" ++ intercalate ", " (map show ts) ++ ")" ++ show rt
        Typedef s     -> show s
        UnsafePtr     -> "*"


isInt x               = x `elem` [I8, I16, I32, I64]

isFloat x             = x `elem` [F32, F64]

isRange (Range _)     = True
isRange _             = False

isArray (Array _ _)   = True
isArray _             = False

isTuple (Tuple _)     = True
isTuple _             = False

isTable (Table _)     = True
isTable _             = False

isSparse (Sparse _)   = True
isSparse _            = False

isTypedef (Typedef _) = True
isTypedef _           = False

isFunc (Func _ _)     = True
isFunc _              = False

isADT (ADT _)         = True
isADT _               = False

isTypeId (Type _)     = True
isTypeId _            = False

isIntegral x          = isInt x || x == Char || x == Enum
isSimple x            = isInt x || isFloat x || x == Char || x == Bool || x == Enum
isAggregate x         = isTuple x || isArray x || isTable x || isFunc x || isSparse x || isRange x
isBase x              = isSimple x || isAggregate x


