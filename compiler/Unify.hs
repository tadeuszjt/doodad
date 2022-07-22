module Unify where

import Data.List
import Control.Monad

import Type as T
import AST as S
import Collect
import Monad
import Error
import Apply


unifyOne :: BoM s m => Constraint -> m [(Int, Type)]
unifyOne (Constraint pos t1 t2) = withPos pos $ case (t1, t2) of
    _ | t1 == t2                   -> return []
    (Type x, t)                    -> return [(x, t)]
    (t, Type x)                    -> return [(x, t)]
    (T.Table tsa, T.Table tsb)
        | length tsa /= length tsb -> fail "length"
        | otherwise                -> unify $ zipWith (Constraint pos) tsa tsb
    (T.Tuple tsa, T.Tuple tsb)
        | length tsa /= length tsb -> fail "length"
        | otherwise                -> unify $ zipWith (Constraint pos) tsa tsb
    _                              -> fail $ "cannot unify " ++ show t1 ++ " with " ++ show t2


unifyOneDefault :: BoM s m => Constraint -> m [(Int, Type)]
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


unify :: BoM s m => [Constraint] -> m [(Int, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (apply subs x)
    return (s ++ subs)


unifyDefault :: BoM s m => [Constraint] -> m [(Int, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    s <- unifyOneDefault (apply subs x)
    return (s ++ subs)

