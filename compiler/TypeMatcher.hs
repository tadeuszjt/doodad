module TypeMatcher where

import qualified Data.Map as Map

import Type
import Symbol


-- t1 is potentially generic?
typesCouldMatch2 :: Map.Map Symbol ([Symbol], Type) -> [Symbol] -> Type -> Type -> Bool
typesCouldMatch2 typeFuncs typeVars t1 t2 = case (flattenTuple t1, flattenTuple t2) of
    (a, b) | a == b        -> True
    (Tuple a, Tuple b)     -> typesCouldMatch2 typeFuncs typeVars a b
    (Record as, Record bs) ->
        length as == length bs &&
        all (== True) (zipWith (typesCouldMatch2 typeFuncs typeVars) as bs)
    (TypeApply s1 ts1, TypeApply s2 ts2) | s1 == s2 ->
        length ts1 == length ts2 &&
        all (== True) (zipWith (typesCouldMatch2 typeFuncs typeVars) ts1 ts2)

    (Record _, _) -> False
    (TypeApply s1 [], TypeApply s2 []) | s1 /= s2 -> False


    (TypeApply s [], I64) -> False
    (TypeApply s [], Bool) -> False
    (TypeApply _ _, Tuple (Record [_, _])) -> False
    (TypeApply s [], Tuple _) -> False
    (I64, I32) -> False

    
    (x, y) -> error $ show (x, y)

    where
        ignoresTuples :: Type -> Bool
        ignoresTuples typ = case typ of
            TypeApply s [] -> case Map.lookup s typeFuncs of
                Just ([], t) -> ignoresTuples t

            Tuple t -> True
            I64 -> True
            Record ts -> False

            _ -> error (show typ)

        flattenTuple :: Type -> Type
        flattenTuple typ = case typ of
            t | isSimple t            -> typ
            TypeApply s ts            -> TypeApply s (map flattenTuple ts)
            Tuple (Record ts)         -> Tuple $ Record (map flattenTuple ts)
            Record ts                 -> Record (map flattenTuple ts)
            Tuple t | ignoresTuples t -> flattenTuple t
            Tuple t | otherwise       -> Tuple (flattenTuple t)
            _ -> error (show typ)
