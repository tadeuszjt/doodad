module Type where

import qualified Data.Map as Map (Map, (!), member)
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
    | Record [Type]
    | Tuple Type
    | TypeApply Symbol Type
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
        Record ts     -> "{" ++ intercalate ", " (map show ts) ++ "}"
        Tuple t       -> "tuple[" ++ show t ++ "]"
        TypeApply s t -> show s ++ "[" ++ show t ++ "]"
--        Key t         -> '@' : show t
--        Range t       -> "[..]" ++ show t
--        Tuple ts      -> "tuple(" ++ intercalate ", " (map show ts) ++ ")"
--        Array n t     -> "[" ++ show n ++ " " ++ show t ++ "]"
--        ADT tss       -> "(" ++ intercalate " | " (map show tss) ++ ")"
--        Table ts      -> "[" ++ intercalate "; " (map show ts) ++ "]"
--        TypeApply s ts -> show s ++ "(" ++ intercalate ", " (map show ts) ++ ")"

isInt x      = x `elem` [U8, I8, I16, I32, I64]
isFloat x    = x `elem` [F32, F64]
isIntegral x = isInt x || x == Char
isSimple x   = isInt x || isFloat x || x == Char || x == Bool || x == String


getTypeSymbol :: MonadFail m => Type ->  m Symbol
getTypeSymbol typ = case typ of
--    TypeApply symbol _ -> return symbol
    _ -> fail $ "no symbol for type: " ++ show typ


findGenerics :: [Symbol] -> Type -> [Type]
findGenerics typeArgs typ = case typ of
    t | isSimple t -> []
    TypeApply s (Record []) | s `elem` typeArgs -> [typ]
    TypeApply s t | s `elem` typeArgs -> error "generic applied to arguments"
    TypeApply s t -> findGenerics typeArgs t
--    Tuple ts -> concat $ map (findGenerics typeArgs) ts
--    Table ts -> concat $ map (findGenerics typeArgs) ts
    Void -> []
    Type _ -> []
    _ -> error $ show typ


-- Replace the matching symbols with a the types specificed in the argument map.
-- [T -> i64]  T -> i64
-- [T -> M]    T[i64] -> M[i64]
applyTypeFunction :: Symbol -> Type -> Type -> Type
applyTypeFunction argSymbol argType typ = case typ of
    TypeApply s (Record []) -> if s == argSymbol then argType else typ
    TypeApply s t | s == argSymbol -> case argType of
        TypeApply s2 (Record []) -> TypeApply s2 t
    Record ts               -> Record $ map (applyTypeFunction argSymbol argType) ts
    Tuple t                 -> Tuple $ applyTypeFunction argSymbol argType t
    _ | isSimple typ        -> typ
    _                       -> error $ "applyTypeFunction: " ++ show typ
    where
--        applyTypeFunctionAdtField :: AdtField -> AdtField
--        applyTypeFunctionAdtField field = case field of
--            FieldType t  -> FieldType $ applyTypeFunction argMap t
--            FieldCtor ts -> FieldCtor $ map (applyTypeFunction argMap) ts
--            FieldNull    -> FieldNull
--            _ -> error $ show field


-- Types could match:
--
-- t0 - anything 
-- t1 - t1
-- [T] T - anything
-- [T] T[something] - anything[something]
typesCouldMatch :: [Symbol] -> Type -> Type -> Bool
typesCouldMatch typeVars a b = case (a, b) of
    (Type _, _)                                    -> True
    (_, Type _)                                    -> True
    (TypeApply s (Record []), _)
        | s `elem` typeVars                        -> True
    (_, TypeApply s (Record []))
        | s `elem` typeVars                        -> True
    (TypeApply s1 t1, TypeApply s2 t2)
        | s1 `elem` typeVars || s2 `elem` typeVars -> typesCouldMatch typeVars t1 t2
    (TypeApply s1 t1, TypeApply s2 t2)             -> symbolsCouldMatch s1 s2 && typesCouldMatch typeVars t1 t2
    (Void, Void)                                   -> True
    _ | isSimple a                                 -> a == b
    _                                              -> error (show (a, b))
    where
        fieldsCouldMatch :: AdtField -> AdtField -> Bool
        fieldsCouldMatch fa fb = case (fa, fb) of
            (FieldNull, FieldNull) -> True
            (FieldType a, FieldType b) -> typesCouldMatch typeVars a b
            _ -> error $ show (fa, fb)
