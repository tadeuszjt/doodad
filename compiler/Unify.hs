{-# LANGUAGE FlexibleContexts #-}
module Unify where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State

import Type as T
import AST as S
import Collect hiding (baseTypeOf)
import Monad
import Error
import Apply
import Symbol


type TypeMap = Map.Map Symbol Type


baseTypeOf :: BoM TypeMap m => Type -> m (Maybe Type)
baseTypeOf typ = case typ of
    T.Typedef symbol -> gets $ Map.lookup symbol
    T.Type x         -> return Nothing
    t                -> return (Just t)


unifyOne :: BoM TypeMap m => (TextPos, Constraint) -> m [(Int, Type)]
unifyOne (pos, constraint) = withPos pos $ case constraint of
    ConsAdtMem t i j agg -> do
        basem <- baseTypeOf agg
        case basem of
            Just (T.ADT tss) -> do
                assert (i < length tss)        "Invalid ADT member"
                assert (j < length (tss !! i)) "Invalid ADT member"
                unifyOne (pos, ConsEq t ((tss !! i) !! j))
            _ -> return []
        
    ConsMember t i agg -> do
        basem <- baseTypeOf agg
        case basem of
            Just (T.Tuple ts) -> unifyOne (pos, ConsEq t $ ts !! i)
            _                 -> return []

    ConsElem t1 t2 -> do
        basem <- baseTypeOf t2
        case basem of
            Just (T.Table [t]) -> unifyOne (pos, ConsEq t1 t)
            Just (T.Array n t) -> unifyOne (pos, ConsEq t1 t)
            _ -> return []

    ConsBase t1 t2 -> do
        base1m <- baseTypeOf t1
        base2m <- baseTypeOf t2
        case (base1m, base2m) of
            (Just b1, Just b2) -> unifyOne (pos, ConsEq b1 b2)
            _                  -> return []

    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                    -> return []
        (Type x, t)                     -> return [(x, t)]
        (t, Type x)                     -> return [(x, t)]
        (T.Table tsa, T.Table tsb)
            | length tsa /= length tsb  -> fail "length"
            | otherwise                 -> unify $ zipWith (\a b -> (pos, ConsEq a b)) tsa tsb
        (T.Tuple tsa, T.Tuple tsb)
            | length tsa /= length tsb  -> fail "length"
            | otherwise                 -> unify $ zipWith (\a b -> (pos, ConsEq a b)) tsa tsb
        (T.UnsafePtr ta, T.UnsafePtr tb)-> unifyOne (pos, ConsEq ta tb)
        _                               -> fail $ "cannot unify " ++ show t1 ++ " with " ++ show t2


unify2 :: BoM TypeMap m => [(TextPos, Constraint)] -> m [(Int, Type)]
unify2 constraints = do
    --liftIO $ putStrLn $ "unify2"
    subs <- unify constraints
    reduced <- reduceSubs subs

    let constraints' = map (\(p, c) -> (p, apply reduced c)) constraints
    if constraints' == constraints then
        return reduced
    else do
        subs' <- unify2 constraints'
        return $ Set.toList $ Set.union (Set.fromList reduced) (Set.fromList subs')

    where
        reduceSubs :: BoM TypeMap m => [(Int, Type)] -> m [(Int, Type)]
        reduceSubs subs = do
            --liftIO $ putStrLn $ "reduceSubs"
            let subs' = Set.toList $ Set.fromList subs
            let subs'' = map (\(i, t) -> (i, apply subs' t)) subs'
            let subs''' = Set.toList $ Set.fromList subs''
            if subs''' == subs then
                return subs'''
            else
                reduceSubs subs'''
        



unify :: BoM TypeMap m => [(TextPos, Constraint)] -> m [(Int, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (fst x, apply subs (snd x))
    return (s ++ subs)


unifyOneDefault :: BoM TypeMap m => (TextPos, Constraint) -> m [(Int, Type)]
unifyOneDefault (pos, ConsEq t1 t2) = withPos pos $ case (t1, t2) of
    _ | t1 == t2                   -> return []
    (Type x, t)                    -> return [(x, t)]
    (t, Type x)                    -> return [(x, t)]
    (T.Table tsa, T.Table tsb)
        | length tsa /= length tsb -> fail "length"
        | otherwise                -> unifyDefault $ zipWith (\a b -> (pos, ConsEq a b)) tsa tsb
    (T.Tuple tsa, T.Tuple tsb)
        | length tsa /= length tsb -> fail "length"
        | otherwise                -> unifyDefault $ zipWith (\a b -> (pos, ConsEq a b)) tsa tsb
    (ta, tb) | ta /= tb            -> return [] -- ignore errors
    _                              -> fail $ "unifyOneDefault: " ++ show (t1, t2)


unifyDefault :: BoM TypeMap m => [(TextPos, Constraint)] -> m [(Int, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    s <- unifyOneDefault (fst x, apply subs (snd x))
    return (s ++ subs)

