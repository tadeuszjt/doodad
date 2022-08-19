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


unifyOne :: BoM TypeMap m => Constraint -> m [(Int, Type)]
unifyOne constraint = withPos (textPos constraint) $ case constraint of
    ConsMember pos t i agg -> do
        basem <- baseTypeOf agg
        case basem of
            Just (T.Tuple ts) -> unifyOne (Constraint pos t $ ts !! i)
            _                 -> return []

    ConsElem pos t1 t2 -> do
        basem <- baseTypeOf t2
        case basem of
            Just (T.Table [t]) -> unifyOne (Constraint pos t1 t)
            Just (T.Array n t) -> unifyOne (Constraint pos t1 t)
            _ -> return []

    ConsBase pos t1 t2 -> do
        base1m <- baseTypeOf t1
        base2m <- baseTypeOf t2
        case (base1m, base2m) of
            (Just b1, Just b2) -> unifyOne $ Constraint pos b1 b2
            _                  -> return []

    Constraint pos t1 t2 -> case (t1, t2) of
        _ | t1 == t2                    -> return []
        (Type x, t)                     -> return [(x, t)]
        (t, Type x)                     -> return [(x, t)]
        (T.Table tsa, T.Table tsb)
            | length tsa /= length tsb  -> fail "length"
            | otherwise                 -> unify $ zipWith (Constraint pos) tsa tsb
        (T.Tuple tsa, T.Tuple tsb)
            | length tsa /= length tsb  -> fail "length"
            | otherwise                 -> unify $ zipWith (Constraint pos) tsa tsb
        (T.UnsafePtr ta, T.UnsafePtr tb)-> unifyOne (Constraint pos ta tb)
        _                               -> fail $ "cannot unify " ++ show t1 ++ " with " ++ show t2


unify2 :: BoM TypeMap m => [Constraint] -> m [(Int, Type)]
unify2 constraints = do
    --liftIO $ putStrLn $ "unify2"
    subs <- unify constraints
    reduced <- reduceSubs subs

    let constraints' = map (apply reduced) constraints
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
        



unify :: BoM TypeMap m => [Constraint] -> m [(Int, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (apply subs x)
    return (s ++ subs)


unifyOneDefault :: BoM TypeMap m => Constraint -> m [(Int, Type)]
unifyOneDefault (Constraint pos t1 t2) = withPos pos $ case (t1, t2) of
    _ | t1 == t2                   -> return []
    (Type x, t)                    -> return [(x, t)]
    (t, Type x)                    -> return [(x, t)]
    (T.Table tsa, T.Table tsb)
        | length tsa /= length tsb -> fail "length"
        | otherwise                -> unifyDefault $ zipWith (Constraint pos) tsa tsb
    (T.Tuple tsa, T.Tuple tsb)
        | length tsa /= length tsb -> fail "length"
        | otherwise                -> unifyDefault $ zipWith (Constraint pos) tsa tsb
    (ta, tb) | ta /= tb            -> return [] -- ignore errors
    _                              -> fail $ "unifyOneDefault: " ++ show (t1, t2)


unifyDefault :: BoM TypeMap m => [Constraint] -> m [(Int, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    s <- unifyOneDefault (apply subs x)
    return (s ++ subs)

