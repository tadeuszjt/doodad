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
    | Tuple [Type]           
    | Array Int Type         
    | Table [Type]         
    | ADT [AdtField]
    | Typedef Symbol
    | TypeApply Symbol [Type]
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
        Tuple ts      -> "(" ++ intercalate ", " (map show ts) ++ ")"
        Array n t     -> "[" ++ show n ++ " " ++ show t ++ "]"
        ADT tss       -> "{" ++ intercalate " | " (map show tss) ++ "}"
        Table ts      -> "[" ++ intercalate "; " (map show ts) ++ "]"
        Typedef s     -> show s

isInt x      = x `elem` [U8, I8, I16, I32, I64]
isFloat x    = x `elem` [F32, F64]
isIntegral x = isInt x || x == Char
isSimple x   = isInt x || isFloat x || x == Char || x == Bool || x == String


typesCouldMatch :: Type -> Type -> Bool
typesCouldMatch a b = case (a, b) of
    (Type _, _)            -> True
    (_, Type _)            -> True
    (Void, Void)           -> True
    (Typedef _, Typedef _) -> a == b
    _ | isSimple a         -> a == b
    (Table as, Table bs)   -> length as == length bs && (all (== True) $ zipWith typesCouldMatch as bs)
    (ADT afs, ADT bfs)     -> length afs == length bfs && (all (== True) $ zipWith fieldsCouldMatch afs bfs)
    (Tuple as, Tuple bs)   -> length as == length bs && (all (== True) $ zipWith typesCouldMatch as bs)

    (_, _) -> False -- TODO
    _                      -> error (show (a, b))
    where
        fieldsCouldMatch :: AdtField -> AdtField -> Bool
        fieldsCouldMatch fa fb = case (fa, fb) of
            (FieldNull, FieldNull) -> True
            (FieldType a, FieldType b) -> typesCouldMatch a b
            _ -> error $ show (fa, fb)
