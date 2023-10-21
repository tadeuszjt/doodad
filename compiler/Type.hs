module Type where

import Data.Maybe
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
    | Table Type
    | TypeApply Symbol [Type]
    deriving (Eq, Ord)

instance Show AdtField where
    show adtField = case adtField of
        FieldNull -> "null"
        FieldCtor ts -> "(" ++ intercalate ", " (map show ts) ++ ")"
        FieldType t -> show t

instance Show Type where
    show t = case t of
        Type id           -> "t" ++ show id
        Void              -> "void"
        U8                -> "u8"
        I8                -> "i8"
        I16               -> "i16"
        I32               -> "i32"
        I64               -> "i64"
        F32               -> "f32"
        F64               -> "f64"
        Bool              -> "bool"
        Char              -> "char"
        String            -> "string"
        Record ts         -> "{" ++ intercalate ", " (map show ts) ++ "}"
        Tuple (Record ts) -> "(" ++ intercalate ", " (map show ts) ++ ")"
        Tuple t           -> "()" ++ show t
        Table t           -> "[]" ++ show t
        TypeApply s []    -> show s
        TypeApply s ts    -> show s ++ "(" ++ intercalate ", " (map show ts) ++ ")"


isInt x      = x `elem` [U8, I8, I16, I32, I64]
isFloat x    = x `elem` [F32, F64]
isIntegral x = isInt x || x == Char
isSimple x   = isInt x || isFloat x || x == Char || x == Bool || x == String


getTypeSymbol :: MonadFail m => Type ->  m Symbol
getTypeSymbol typ = case typ of
    TypeApply symbol _ -> return symbol
    _ -> fail $ "no symbol for type: " ++ show typ


findGenerics :: [Symbol] -> Type -> [Type]
findGenerics typeArgs typ = case typ of
    t | isSimple t -> []
    TypeApply s [] | s `elem` typeArgs -> [typ]
    TypeApply s _  | s `elem` typeArgs -> error "generic applied to arguments"
    TypeApply s ts -> concat $ map (findGenerics typeArgs) ts
    Table t -> findGenerics typeArgs t
    Void -> []
    Type _ -> []
    Record ts -> concat $ map (findGenerics typeArgs) ts
    Tuple t -> findGenerics typeArgs t
    _ -> error $ show typ


applyTypeFunction :: [Symbol] -> [Type] -> Type -> Type
applyTypeFunction argSymbols argTypes typ = case length argSymbols == length argTypes of
    False -> error "invalid arguments to applyTypeFunction"
    True -> let args = zip argSymbols argTypes in case typ of
        TypeApply s [] -> if isJust (lookup s args) then fromJust (lookup s args) else typ
    --    TypeApply s t | isJust (lookup s args) -> case fromJust (lookup s args) of
    --        TypeApply s2 (Record []) -> TypeApply s2 t
        Record ts               -> Record $ map (applyTypeFunction argSymbols argTypes) ts
        Tuple t                 -> Tuple $ applyTypeFunction argSymbols argTypes t
        Table t                 -> Table $ applyTypeFunction argSymbols argTypes t
        _ | isSimple typ        -> typ
        _                       -> error $ "applyTypeFunction: " ++ show typ



-- Types could match:
--
-- t0 - anything 
-- t1 - t1
-- [T] T - anything
-- [T] T[something] - anything[something]
--
typesCouldMatch :: [Symbol] -> Type -> Type -> Bool
typesCouldMatch typeVars a b = case (a, b) of
    (Type _, _)                                    -> True
    (_, Type _)                                    -> True

    (TypeApply s1 ts1, TypeApply s2 ts2)
        | length ts1 == length ts2 && (s1 `elem` typeVars || s2 `elem` typeVars || symbolsCouldMatch s1 s2) -> 
            all (== True) $ zipWith (typesCouldMatch typeVars) ts1 ts2
        | otherwise -> False

    (TypeApply s1 [], _)
        | s1 `elem` typeVars -> True
        | otherwise          -> False
    (_, TypeApply s1 [])
        | s1 `elem` typeVars -> True
        | otherwise          -> False

    (Record ts1, Record ts2) ->
        length ts1 == length ts2 && (all (== True) $ zipWith (typesCouldMatch typeVars) ts1 ts2)

    (Table t1, Table t2)                           -> typesCouldMatch typeVars t1 t2
    (Tuple t1, Tuple t2)                           -> typesCouldMatch typeVars t1 t2

    (Record _, Tuple _) -> False


    (Void, Void)                                   -> True
    _ | isSimple a                                 -> a == b
    _                                              -> error (show (a, b))
    where
        fieldsCouldMatch :: AdtField -> AdtField -> Bool
        fieldsCouldMatch fa fb = case (fa, fb) of
            (FieldNull, FieldNull) -> True
            (FieldType a, FieldType b) -> typesCouldMatch typeVars a b
            _ -> error $ show (fa, fb)
