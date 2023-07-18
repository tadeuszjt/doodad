module Type where

import Data.List
import Symbol

class Typeof a where typeof :: a -> Type
instance Typeof Type where 
    typeof typ = typ

data AdtField
    = FieldNull
    | FieldType Type
    | FieldCtor [Type]
    deriving (Eq, Ord)

data Type
    = Type Int
    | Void
    | U8
    | I8                     
    | I16                    
    | I32                    
    | I64                    
    | F32                    
    | F64                    
    | Bool                   
    | Char                   
    | String
    | Key Type
    | Range Type
    | Sparse [Type]
    | Map Type Type
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
        U8            -> "u8"
        I8            -> "i8"
        I16           -> "i16"
        I32           -> "i32"
        I64           -> "i64"
        F32           -> "f32"
        F64           -> "f64"
        Bool          -> "bool"
        Char          -> "char"
        String        -> "string"
        Key t         -> '@' : show t
        Range t       -> "[..]" ++ show t
        Sparse ts     -> "sparse" ++ "[" ++ intercalate "; " (map show ts) ++ "]"
        Map tk tv     -> "map" ++ "[" ++ show tk ++ "]" ++ show tv
        Tuple ts      -> "(" ++ intercalate ", " (map show ts) ++ ")"
        Array n t     -> "[" ++ show n ++ " " ++ show t ++ "]"
        ADT tss       -> "{" ++ intercalate " | " (map show tss) ++ "}"
        Table ts      -> "[" ++ intercalate "; " (map show ts) ++ "]"
        Func ts rt    -> "fn(" ++ intercalate ", " (map show ts) ++ ")" ++ show rt
        Typedef s     -> show s
        UnsafePtr     -> "*"

isInt x      = x `elem` [U8, I8, I16, I32, I64]
isFloat x    = x `elem` [F32, F64]
isIntegral x = isInt x || x == Char
isSimple x   = isInt x || isFloat x || x == Char || x == Bool || x == String


