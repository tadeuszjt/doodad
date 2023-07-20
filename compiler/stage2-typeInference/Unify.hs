{-# LANGUAGE FlexibleContexts #-}
module Unify where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State

import Type
import qualified AST as S
import Collect hiding (baseTypeOf)
import Monad
import Error
import Apply
import Symbol


type TypeMap = Map.Map Symbol Type


baseTypeOf :: BoM TypeMap m => Type -> m (Maybe Type)
baseTypeOf typ = case typ of
    Typedef symbol -> do
        resm <- gets $ Map.lookup symbol
        assert (isJust resm) $ "Cannot find symbol: " ++ show symbol
        baseTypeOf (fromJust resm)

    Type x         -> return Nothing
    t              -> return (Just t)


unifyOne :: BoM TypeMap m => TextPos -> Constraint -> m [(Int, Type)]
unifyOne pos constraint = withPos pos $ case constraint of
    ConsAdtMem t i j agg -> do
        basem <- baseTypeOf agg
        case basem of
            Just (ADT fs) -> do
                assert (i < length fs)        "Invalid ADT member"
                case fs !! i of
                    FieldNull -> fail "Invalid ADT member"
                    FieldType ft -> do
                        assert (j == 0) "Invalid ADT member"
                        unifyOne pos $ ConsEq t ft
                    FieldCtor ts -> do
                        assert (j < length ts) "Invalid ADT member"
                        unifyOne pos $ ConsEq t (ts !! j)
            _ -> return []
        
    ConsField t i agg -> do
        basem <- baseTypeOf agg
        case basem of
            Just (Tuple ts) -> unifyOne pos (ConsEq t $ ts !! i)
            Just (Table ts) -> unifyOne pos (ConsEq t $ Table [ts !! i])
            _               -> return []

    ConsMember t1 i t2 -> do
        basem <- baseTypeOf t1
        case basem of
            Just (Table ts)  -> unifyOne pos (ConsEq t2 $ ts !! i)
            Just (Sparse ts) -> unifyOne pos (ConsEq t2 $ ts !! i)
            Just (Array n t) -> do 
                assert (i == 0) "ConsMember: Invalid index"
                unifyOne pos (ConsEq t2 t)
            Just (Range t)   -> do 
                assert (i == 0) "ConsMember: Invalid index"
                unifyOne pos (ConsEq t2 t)
            Just String -> do
                unifyOne pos (ConsEq t2 Char)
            Nothing -> return []
            _ -> error (show basem)

    ConsElem t1 t2 -> do
        basem <- baseTypeOf t1
        case basem of
            Just (Table [t])  -> unifyOne pos (ConsEq t2 t)
            Just (Table ts)  -> unifyOne pos (ConsEq t2 $ Tuple ts)
            Just (Array n t) -> unifyOne pos (ConsEq t2 t)
            Nothing -> return []
            _ -> error (show basem)

    ConsSubscript t1 t2 -> do
        basem <- baseTypeOf t2
        case basem of
            Just (Table [t]) -> unifyOne pos (ConsEq t1 t)
            Just (Table ts)  -> unifyOne pos (ConsBase t1 $ Tuple ts)
            Just (Sparse [t]) -> unifyOne pos (ConsEq t1 t)
            Just (Array n t) -> unifyOne pos (ConsEq t1 t)
            Just (Range t)   -> unifyOne pos (ConsEq t1 Bool)
            Just String      -> unifyOne pos (ConsEq t1 Char)
            _ -> return []

    ConsBase t1 t2 -> do
        base1m <- baseTypeOf t1
        base2m <- baseTypeOf t2
        case (base1m, base2m) of
            (Just b1, Just b2) -> unifyOne pos (ConsEq b1 b2)
            _                  -> return []

    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                    -> return []
        (Type x, t)                     -> return [(x, t)]
        (t, Type x)                     -> return [(x, t)]
        (Table tsa, Table tsb)
            | length tsa /= length tsb  -> fail "length"
            | otherwise                 -> unify $ zipWith (\a b -> (ConsEq a b, pos)) tsa tsb
        (Tuple tsa, Tuple tsb)
            | length tsa /= length tsb  -> fail "length"
            | otherwise                 -> unify $ zipWith (\a b -> (ConsEq a b, pos)) tsa tsb
        --(UnsafePtr ta, UnsafePtr tb)    -> unifyOne pos (ConsEq ta tb)
        (Range a, Range b)              -> unifyOne pos $ ConsEq a b
        _                               -> fail $ "cannot unify " ++ show t1 ++ " with " ++ show t2


unify2 :: BoM TypeMap m => [(Constraint, TextPos)] -> m [(Int, Type)]
unify2 constraints = do
    --liftIO $ putStrLn $ "unify2"
    subs <- unify constraints
    reduced <- reduceSubs subs

    let constraints' = map (\(c, p) -> (applySubs reduced c, p)) constraints
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
            let subs'' = map (\(i, t) -> (i, applySubs subs' t)) subs'
            let subs''' = Set.toList $ Set.fromList subs''
            if subs''' == subs then
                return subs'''
            else
                reduceSubs subs'''
        


unify :: BoM TypeMap m => [(Constraint, TextPos)] -> m [(Int, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (snd x) $ applySubs subs (fst x)
    return (s ++ subs)


unifyOneDefault :: BoM TypeMap m => (TextPos, Constraint) -> m [(Int, Type)]
unifyOneDefault (pos, ConsEq t1 t2) = withPos pos $ case (t1, t2) of
    _ | t1 == t2                   -> return []
    (Type x, t)                    -> return [(x, t)]
    (t, Type x)                    -> return [(x, t)]
    (Table tsa, Table tsb)
        | length tsa /= length tsb -> fail "length"
        | otherwise                -> unifyDefault $ zipWith (\a b -> (ConsEq a b, pos)) tsa tsb
    (Tuple tsa, Tuple tsb)
        | length tsa /= length tsb -> fail "length"
        | otherwise                -> unifyDefault $ zipWith (\a b -> (ConsEq a b, pos)) tsa tsb
    (ta, tb) | ta /= tb            -> return [] -- ignore errors
    _                              -> fail $ "unifyOneDefault: " ++ show (t1, t2)


unifyDefault :: BoM TypeMap m => [(Constraint, TextPos)] -> m [(Int, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    s <- unifyOneDefault (snd x, applySubs subs (fst x))
    return (s ++ subs)

