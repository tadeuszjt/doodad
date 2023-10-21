module TypeMatcher where

import qualified Data.Map as Map

import Type
import Symbol


typesCouldMatch2 :: Map.Map Symbol ([Symbol], Type) -> [Symbol] -> Type -> Type -> Bool
typesCouldMatch2 typedefs generics t1 t2 = case (flattenTuple t1, flattenTuple t2) of
    (a, b) | a == b                   -> True
    (Type _, _)                       -> True
    (_, Type _)                       -> True
    (Tuple a, Tuple b)                -> typesCouldMatch2 typedefs generics a b
    (Table a, Table b)                -> typesCouldMatch2 typedefs generics a b
    (Record as, Record bs)            ->
        length as == length bs &&
        all (== True) (zipWith (typesCouldMatch2 typedefs generics) as bs)

    -- type variables
    (TypeApply s1 ts1, TypeApply s2 ts2)
        | s1 `elem` generics && s2 `elem` generics ->
            length ts1 == length ts2 &&
            all (== True) (zipWith (typesCouldMatch2 typedefs generics) ts1 ts2)
    (x, TypeApply s ts) | s `elem` generics -> True
    (TypeApply s ts, x) | s `elem` generics -> True

    -- type defs
    (TypeApply s1 ts1, TypeApply s2 ts2) | s1 == s2 ->
        length ts1 == length ts2 &&
        all (== True) (zipWith (typesCouldMatch2 typedefs generics) ts1 ts2)

    _ -> False

    where
        definitelyIgnoresTuples :: Type -> Bool
        definitelyIgnoresTuples typ = case typ of
            t | isSimple t -> True
            Tuple t        -> True
            Table t        -> True
            Record ts      -> False
            TypeApply s ts | s `elem` generics      -> False
            TypeApply s ts | Map.member s typedefs ->
                let (ss, t) = typedefs Map.! s in
                definitelyIgnoresTuples (applyTypeFunction ss ts t)
            _ -> error (show typ)


        flattenTuple :: Type -> Type
        flattenTuple typ = case typ of
            t | isSimple t                      -> typ
            Table t                             -> Table (flattenTuple t)
            Void                                -> typ
            Type _                              -> typ
            TypeApply s ts                      -> TypeApply s (map flattenTuple ts)
            Record ts                           -> Record (map flattenTuple ts)
            Tuple t | definitelyIgnoresTuples t -> flattenTuple t
            Tuple t | otherwise                 -> Tuple (flattenTuple t)
            _ -> error (show typ)
