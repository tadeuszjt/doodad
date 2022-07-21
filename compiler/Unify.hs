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
    (Type x, t)                          -> return [(x, t)]
    (t, Type x)                          -> return [(x, t)]
    (T.Bool, T.Bool)                     -> return []
    (I64, I64)                           -> return []
    (Void, Void)                         -> return []
    (F32, F32)                           -> return []
    (F64, F64)                           -> return []
    (T.Char, T.Char)                     -> return []
    (T.Table [T.Char], T.Table [T.Char]) -> return []
    (T.Tuple tsa, T.Tuple tsb)
        | length tsa /= length tsb       -> fail "length"
        | otherwise                      -> concat <$> zipWithM (\ta tb -> unifyOne (Constraint pos ta tb)) tsa tsb
    (T.Typedef s1, T.Typedef s2)
        | s1 == s2                       -> return []
    _                                    -> fail $ "unify: " ++ show (t1, t2)


unify :: BoM s m => [Constraint] -> m [(Int, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (apply subs x)
    return (s ++ subs)


unifyOneDefault :: BoM s m => Constraint -> m [(Int, Type)]
unifyOneDefault (Constraint pos t1 t2) = withPos pos $ case (t1, t2) of
    (Type x, t)                          -> return [(x, t)]
    (t, Type x)                          -> return [(x, t)]
    (T.Bool, T.Bool)                     -> return []
    (I64, I64)                           -> return []
    (F32, F32)                           -> return []
    (F64, F64)                           -> return []
    (Void, Void)                         -> return []
    (T.Char, T.Char)                     -> return []
    (T.Table [T.Char], T.Table [T.Char]) -> return []
    (T.Tuple tsa, T.Tuple tsb)
        | length tsa /= length tsb       -> fail "length"
        | otherwise                      -> concat <$> zipWithM (\ta tb -> unifyOneDefault (Constraint pos ta tb)) tsa tsb
    (T.Typedef s1, T.Typedef s2)
        | s1 == s2                       -> return []
    (ta, tb) | ta /= tb                  -> return [] -- ignore errors
    _                                    -> fail $ "unify " ++ show (t1, t2)


unifyDefault :: BoM s m => [Constraint] -> m [(Int, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    s <- unifyOneDefault (apply subs x)
    return (s ++ subs)

