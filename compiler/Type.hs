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


isInt :: Type -> Bool
isInt typ = case typ of
    U8 -> True
    I8 -> True
    I16 -> True
    I32 -> True
    I64 -> True
    _ -> False

isFloat :: Type -> Bool
isFloat typ = case typ of
    F32 -> True
    F64 -> True
    _   -> False

isSimple :: Type -> Bool
isSimple typ = case typ of
    U8 -> True
    I8 -> True
    I16 -> True
    I32 -> True
    I64 -> True
    F32 -> True
    F64 -> True
    Bool -> True
    String -> True
    Char -> True
    _ -> False

isIntegral x = isInt x || x == Char


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
getRecordTypes :: TypeDefs -> Type -> [Type]
getRecordTypes typeDefs typ = case typ of
    Record ts -> concat $ map (getRecordTypes typeDefs) ts
    TypeApply symbol ts -> case Map.lookup symbol typeDefs of
        Just (ss, t) -> case applyTypeArguments ss ts t of
            Record xs -> concat $ map (getRecordTypes typeDefs) xs
            x         -> getRecordTypes typeDefs x

    t | isSimple t -> [t]
    Table _ -> [typ]
    Tuple _ -> [typ]
    _ -> error (show typ)


data RecordTree
    = RecordLeaf Type Int
    | RecordTree [RecordTree]

instance Show RecordTree where
    show (RecordLeaf t i) = show t ++ "." ++ show i
    show (RecordTree rs ) = "{" ++ intercalate ", " (map show rs) ++ "}"

getRecordTree :: TypeDefs -> Type -> RecordTree
getRecordTree typeDefs typ = case typ of
    Record ts           -> RecordTree $ applyFunc 0 ts
    TypeApply symbol ts -> case Map.lookup symbol typeDefs of
        Just (ss, Tuple t)   -> RecordLeaf (applyTypeArguments ss ts (Tuple t)) 0
        Just (ss, Record xs) -> getRecordTree typeDefs $ applyTypeArguments ss ts (Record xs)
        x -> error (show x)

    _ -> error (show typ)
    where
        applyFunc :: Int -> [Type] -> [RecordTree]
        applyFunc offset ts = case ts of
            []     -> []
            (x:xs) -> let tree = getRecordTree' offset x in
                tree : (applyFunc (getMax tree + 1) xs)

        getMax :: RecordTree -> Int
        getMax tree = case tree of
            RecordLeaf _ i -> i
            RecordTree ns  -> getMax (last ns)
            _ -> error (show tree)


        getRecordTree' :: Int -> Type -> RecordTree
        getRecordTree' offset typ = case typ of
            t | isSimple t -> RecordLeaf typ offset
            Table _        -> RecordLeaf typ offset
            Tuple _        -> RecordLeaf typ offset
            Record ts      -> RecordTree (applyFunc offset ts)
            TypeApply symbol ts -> case Map.lookup symbol typeDefs of
                Just (ss, t) -> getRecordTree' offset (applyTypeArguments ss ts t)
                x -> error (show x)
            _ -> error (show typ)


getRecordLeaves :: TypeDefs -> RecordTree -> [ (Type, Int) ]
getRecordLeaves typeDefs tree = case tree of
    RecordTree ns  -> concat $ map (getRecordLeaves typeDefs) ns
    RecordLeaf t i -> [ (t, i) ]
    x -> error (show x)


getTypeFieldIndex :: TypeDefs -> Type -> Type -> Int
getTypeFieldIndex typeDefs typ field = case typ of
    Tuple (Record ts) -> fromJust (elemIndex field ts)
    Tuple (TypeApply symbol ts) -> case Map.lookup symbol typeDefs of
        Just (ss, t) -> getTypeFieldIndex typeDefs (applyTypeArguments ss ts t) field

    Record ts -> fromJust (elemIndex field ts)
    TypeApply symbol ts -> case Map.lookup symbol typeDefs of
        Just (ss, t) -> getTypeFieldIndex typeDefs (applyTypeArguments ss ts t) field
    _ -> error (show typ)

