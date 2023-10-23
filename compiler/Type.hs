module Type where

import Data.Maybe
import qualified Data.Map as Map (Map, (!), member)
import Data.List
import Symbol

class Typeof a where typeof :: a -> Type
instance Typeof Type where 
    typeof typ = typ

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
    | Range Type
    | Record [Type]
    | Tuple Type
    | Table Type
    | ADT [Type]
    | TypeApply Symbol [Type]
    deriving (Eq, Ord)

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
        Range t           -> "[..]" ++ show t
        Record ts         -> "{" ++ intercalate ", " (map show ts) ++ "}"
        Tuple (Record ts) -> "(" ++ intercalate ", " (map show ts) ++ ")"
        Tuple t           -> "()" ++ show t
        Table t           -> "[]" ++ show t
        ADT ts            -> "(" ++ intercalate " | " (map show ts) ++ ")"
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
        ADT ts                  -> ADT $ map (applyTypeFunction argSymbols argTypes) ts
        _ | isSimple typ        -> typ
        _                       -> error $ "applyTypeFunction: " ++ show typ



typesCouldMatch :: Map.Map Symbol ([Symbol], Type) -> [Symbol] -> Type -> Type -> Bool
typesCouldMatch typedefs generics t1 t2 = case (flattenTuple typedefs t1, flattenTuple typedefs t2) of
    (a, b) | a == b                   -> True
    (Type _, _)                       -> True
    (_, Type _)                       -> True
    (Tuple a, Tuple b)                -> typesCouldMatch typedefs generics a b
    (Table a, Table b)                -> typesCouldMatch typedefs generics a b
    (Record as, Record bs)            ->
        length as == length bs &&
        all (== True) (zipWith (typesCouldMatch typedefs generics) as bs)

    -- type variables
    (TypeApply s1 ts1, TypeApply s2 ts2)
        | s1 `elem` generics && s2 `elem` generics ->
            length ts1 == length ts2 &&
            all (== True) (zipWith (typesCouldMatch typedefs generics) ts1 ts2)
    (x, TypeApply s ts) | s `elem` generics -> True
    (TypeApply s ts, x) | s `elem` generics -> True

    -- type defs
    (TypeApply s1 ts1, TypeApply s2 ts2) | s1 == s2 ->
        length ts1 == length ts2 &&
        all (== True) (zipWith (typesCouldMatch typedefs generics) ts1 ts2)

    _ -> False


definitelyIgnoresTuples :: Map.Map Symbol ([Symbol], Type) -> Type -> Bool
definitelyIgnoresTuples typedefs typ = case typ of
    t | isSimple t -> True
    Tuple t        -> True
    Table t        -> True
    Record ts      -> False
    Type _         -> False
    TypeApply s ts | Map.member s typedefs ->
        let (ss, t) = typedefs Map.! s in
        definitelyIgnoresTuples typedefs (applyTypeFunction ss ts t)
    TypeApply s ts -> False -- must be generic
    _ -> error (show typ)


flattenTuple :: Map.Map Symbol ([Symbol], Type) -> Type -> Type
flattenTuple typedefs typ = case typ of
    t | isSimple t                      -> typ
    Table t                             -> Table (flattenTuple typedefs t)
    Void                                -> typ
    Type _                              -> typ
    TypeApply s ts                      -> TypeApply s (map (flattenTuple typedefs) ts)
    Record ts                           -> Record (map (flattenTuple typedefs) ts)
    ADT ts                              -> ADT (map (flattenTuple typedefs) ts)
    Range t                             -> Range (flattenTuple typedefs t)
    Tuple t | definitelyIgnoresTuples typedefs t -> flattenTuple typedefs t
    Tuple t | otherwise                 -> Tuple (flattenTuple typedefs t)
    _ -> error (show typ)
