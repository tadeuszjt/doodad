module Type where

import Data.Maybe
import qualified Data.Map as Map (Map, (!), member, lookup)
import Data.List
import Symbol

type TypeDefs = Map.Map Symbol ([Symbol], Type)

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


applyTypeArguments :: [Symbol] -> [Type] -> Type -> Type
applyTypeArguments argSymbols argTypes typ = case length argSymbols == length argTypes of
    False -> error $ "invalid arguments to applyTypeArguments for: " ++ show typ
    True -> let args = zip argSymbols argTypes in case typ of
        TypeApply s [] -> if isJust (lookup s args) then fromJust (lookup s args) else typ
    --    TypeApply s t | isJust (lookup s args) -> case fromJust (lookup s args) of
    --        TypeApply s2 (Record []) -> TypeApply s2 t
        Record ts               -> Record $ map (applyTypeArguments argSymbols argTypes) ts
        Tuple t                 -> Tuple $ applyTypeArguments argSymbols argTypes t
        Table t                 -> Table $ applyTypeArguments argSymbols argTypes t
        ADT ts                  -> ADT $ map (applyTypeArguments argSymbols argTypes) ts
        _ | isSimple typ        -> typ
        Void                    -> typ
        _                       -> error $ "applyTypeArguments: " ++ show typ


typesCouldMatch :: TypeDefs -> [Symbol] -> Type -> Type -> Bool
typesCouldMatch typedefs generics t1 t2 = typesCouldMatchPure
        typedefs
        generics
        (addTuple (flattenTuple typedefs t1))
        (addTuple (flattenTuple typedefs t2))
    where
        addTuple :: Type -> Type
        addTuple typ = case typ of
            Tuple t                                -> Tuple t
            t | definitelyIgnoresTuples typedefs t -> Tuple t
            t                                      -> t

        typesCouldMatchPure :: Map.Map Symbol ([Symbol], Type) -> [Symbol] -> Type -> Type -> Bool
        typesCouldMatchPure typedefs generics t1 t2 = case (t1, t2) of
            (a, b) | a == b                   -> True
            (Type _, _)                       -> True
            (_, Type _)                       -> True

            (Tuple a, Tuple b)                -> typesCouldMatchPure typedefs generics a b
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


-- Determines whether the type will change after a tuple application.
-- For example:
-- i64         -> False because ()i64 == i64
-- ()string    -> False because ()()string == ()string
-- {i64, bool} -> True  because (){i64, bool} != {i64, bool}
definitelyIgnoresTuples :: TypeDefs -> Type -> Bool
definitelyIgnoresTuples typedefs typ = case typ of
    ADT _          -> True
    Void           -> True
    t | isSimple t -> True
    Tuple t        -> True
    Table t        -> True
    Record ts      -> False
    Type _         -> False
    TypeApply s ts | Map.member s typedefs ->
        let (ss, t) = typedefs Map.! s in
        definitelyIgnoresTuples typedefs (applyTypeArguments ss ts t)
    TypeApply s ts -> False -- must be generic
    _ -> error (show typ)


-- Recursively removes all superfluous tuple applications from the type.
-- For example:
-- ()i64         -> i64
-- ()()string    -> string
-- ()T           -> ()T
-- (){i64, bool} -> (){i64, bool}
flattenTuple :: TypeDefs -> Type -> Type
flattenTuple typedefs typ = case typ of
    t | isSimple t                               -> typ
    Table t                                      -> Table (flattenTuple typedefs t)
    Void                                         -> typ
    Type _                                       -> typ
    TypeApply s ts                               -> TypeApply s (map (flattenTuple typedefs) ts)
    Record ts                                    -> Record (map (flattenTuple typedefs) ts)
    ADT ts                                       -> ADT (map (flattenTuple typedefs) ts)
    Range t                                      -> Range (flattenTuple typedefs t)
    Tuple t | definitelyIgnoresTuples typedefs t -> flattenTuple typedefs t
    Tuple t | otherwise                          -> Tuple (flattenTuple typedefs t)
    _                                            -> error (show typ)


-- Returns the list of types represeted by a tree of records.
-- For example:
-- {i64, bool}             -> [i64, bool] 
-- { {i64, bool}, string } -> [i64, bool, string]
-- { Person, Index }       -> [string, i64, i64]   (Person == {name:string, age:i64})
-- i64                     -> [i64]
getRecordTreeTypes :: TypeDefs -> Type -> [Type]
getRecordTreeTypes typeDefs typ = case typ of
    Record ts -> concat $ map (getRecordTreeTypes typeDefs) ts

    TypeApply symbol ts -> case Map.lookup symbol typeDefs of
        Just (ss, t) -> case applyTypeArguments ss ts t of
            Record xs -> concat $ map (getRecordTreeTypes typeDefs) xs
            x         -> getRecordTreeTypes typeDefs x

    I64 -> [I64]
    Bool -> [Bool]
    String -> [String]
    Table _ -> [typ]
    _ -> error (show typ)



getTypeFieldIndex :: TypeDefs -> Type -> Type -> Int
getTypeFieldIndex typeDefs typ field = case typ of
    Tuple (Record ts) -> fromJust (elemIndex field ts)
    _ -> error (show typ)

