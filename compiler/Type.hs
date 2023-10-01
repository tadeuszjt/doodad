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
    | Key Type
    | Range Type
    | Tuple [Type]           
    | Array Int Type         
    | Table [Type]         
    | ADT [AdtField]
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
        Tuple ts      -> "tuple(" ++ intercalate ", " (map show ts) ++ ")"
        Array n t     -> "[" ++ show n ++ " " ++ show t ++ "]"
        ADT tss       -> "(" ++ intercalate " | " (map show tss) ++ ")"
        Table ts      -> "[" ++ intercalate "; " (map show ts) ++ "]"
        TypeApply s ts -> show s ++ "(" ++ intercalate ", " (map show ts) ++ ")"

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
    TypeApply s [] | s `elem` typeArgs -> [typ]
    TypeApply s ts | s `elem` typeArgs -> error "generic applied to arguments"
    TypeApply s ts -> concat $ map (findGenerics typeArgs) ts
    t | isSimple t -> []
    Type _ -> []
    _ -> error $ show typ


-- Replace the matching symbols with a the types specificed in the argument map.
applyTypeFunction :: Map.Map Symbol Type.Type -> Type.Type -> Type.Type
applyTypeFunction argMap typ = case typ of
    TypeApply s []   -> if Map.member s argMap then argMap Map.! s else typ
    Tuple ts         -> Type.Tuple $ map (applyTypeFunction argMap) ts
    Table ts         -> Type.Table $ map (applyTypeFunction argMap) ts
    ADT fs           -> ADT $ map applyTypeFunctionAdtField fs
    TypeApply _ _    -> error "here"
    _ | isSimple typ -> typ
    _                -> error $ "applyTypeFunction: " ++ show typ
    where
        applyTypeFunctionAdtField :: AdtField -> AdtField
        applyTypeFunctionAdtField field = case field of
            FieldType t  -> FieldType $ applyTypeFunction argMap t
            FieldCtor ts -> FieldCtor $ map (applyTypeFunction argMap) ts
            FieldNull    -> FieldNull
            _ -> error $ show field


typesCouldMatch :: [Symbol] -> Type -> Type -> Bool
typesCouldMatch typeVars a b = case (a, b) of
    (Type _, _)            -> True
    (_, Type _)            -> True
    (TypeApply s1 [], TypeApply s2 []) | s1 `elem` typeVars && s2 `elem` typeVars -> s1 == s2
    (TypeApply s [], _) | s `elem` typeVars -> True
    (_, TypeApply s []) | s `elem` typeVars -> True

    (Void, Void)           -> True
    (TypeApply sa ats, TypeApply sb bts) ->
        symbolsCouldMatch sa sb
        && length ats == length bts
        && (all (== True) $ zipWith (typesCouldMatch typeVars) ats bts)
    _ | isSimple a         -> a == b
    (Table as, Table bs)   ->
        length as == length bs
        && (all (== True) $ zipWith (typesCouldMatch typeVars) as bs)
    (ADT afs, ADT bfs)     ->
        length afs == length bfs
        && (all (== True) $ zipWith fieldsCouldMatch afs bfs)
    (Tuple as, Tuple bs)   ->
        length as == length bs
        && (all (== True) $ zipWith (typesCouldMatch typeVars) as bs)

    (_, _) -> False -- TODO
    _                      -> error (show (a, b))
    where
        fieldsCouldMatch :: AdtField -> AdtField -> Bool
        fieldsCouldMatch fa fb = case (fa, fb) of
            (FieldNull, FieldNull) -> True
            (FieldType a, FieldType b) -> typesCouldMatch typeVars a b
            _ -> error $ show (fa, fb)
