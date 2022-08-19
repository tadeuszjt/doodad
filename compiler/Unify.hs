{-# LANGUAGE FlexibleContexts #-}
module Unify where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
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
    T.Typedef symbol -> do
        gets $ Map.lookup symbol

    T.Type x -> return Nothing

    t -> return (Just t)


unifyOne :: BoM TypeMap m => Constraint -> m [(Int, Type)]
unifyOne (ConsElem pos t1 t2) = withPos pos $ do
    basem <- baseTypeOf t2
    case basem of
        Just (T.Table [t]) -> unifyOne (Constraint pos t1 t)
        Just (T.Array n t) -> unifyOne (Constraint pos t1 t)
        _ -> return []


unifyOne (ConsBase pos t1 t2) = withPos pos $ do
    base1m <- baseTypeOf t1
    base2m <- baseTypeOf t2
    if isJust base1m && isJust base2m then
        unifyOne $ Constraint pos (fromJust base1m) (fromJust base2m)
    else
        return []
    
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
    (T.UnsafePtr ta, T.UnsafePtr tb) -> unifyOne (Constraint pos ta tb)
    _                              -> fail $ "cannot unify " ++ show t1 ++ " with " ++ show t2


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


unify :: BoM TypeMap m => [Constraint] -> m [(Int, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (apply subs x)
    return (s ++ subs)


unifyDefault :: BoM TypeMap m => [Constraint] -> m [(Int, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    s <- unifyOneDefault (apply subs x)
    return (s ++ subs)

