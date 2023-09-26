{-# LANGUAGE FlexibleContexts #-}
module Unify where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Type
import qualified AST as S
import Constraint
import Monad
import Error
import Apply
import Symbol
import Collect


data UnifyState
    = UnifyState
        { typeMap      :: Map.Map Symbol Collect.Object
        }


baseTypeOf :: BoM UnifyState m => Type -> m (Maybe Type)
baseTypeOf typ = case typ of
    TypeApply symbol ts -> do
        resm <- gets $ Map.lookup symbol . typeMap
        case resm of
            Nothing                 -> return Nothing
            Just (ObjTypeFunc ss t) -> do
                assert (length ts == length ss) "invalid type function args"
                baseTypeOf $ applyTypeFunction (Map.fromList $ zip ss ts) t
    Type x -> return Nothing
    t      -> return (Just t)


unifyOne :: BoM UnifyState m => TextPos -> Constraint -> m [(Type, Type)]
unifyOne pos constraint = withPos pos $ case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                    -> return []
        (Type x, t)                     -> return [(Type x, t)]
        (t, Type x)                     -> return [(Type x, t)]
        (Table tsa, Table tsb)
            | length tsa /= length tsb  -> fail "length"
            | otherwise                 -> unify $ zipWith (\a b -> (ConsEq a b, pos)) tsa tsb
        (Tuple tsa, Tuple tsb)
            | length tsa /= length tsb  -> fail "length"
            | otherwise                 -> unify $ zipWith (\a b -> (ConsEq a b, pos)) tsa tsb
        (Range a, Range b)              -> unifyOne pos $ ConsEq a b
        _                               -> fail $ "cannot unify " ++ show t1 ++ " with " ++ show t2

    ConsAdtMem t i j agg -> do
        basem <- baseTypeOf agg
        case basem of
            Just (ADT fs) -> do
                assert (i < length fs) "Invalid ADT member"
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
            Just (Tuple ts)  -> unifyOne pos (ConsEq t2 $ ts !! i)
            Just (Table ts)  -> unifyOne pos (ConsEq t2 $ ts !! i)
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
            Just (Table [t]) -> unifyOne pos (ConsEq t2 t)
            Just (Table ts)  -> unifyOne pos (ConsEq t2 $ Tuple ts)
            Just (Array n t) -> unifyOne pos (ConsEq t2 t)
            Nothing -> return []
            _ -> error (show basem)

    ConsSubscript t1 t2 -> do
        basem <- baseTypeOf t1
        case basem of
            Just (Table [t])  -> unifyOne pos (ConsEq t2 t)
            Just (Table ts)   -> unifyOne pos (ConsEq t2 $ Tuple ts)
            Just (Array n t)  -> unifyOne pos (ConsEq t2 t)
            Just String       -> unifyOne pos (ConsEq t2 Char)
            _ -> return []

    ConsBase t1 t2 -> do
        base1m <- baseTypeOf t1
        base2m <- baseTypeOf t2
        case (base1m, base2m) of
            (Just b1, Just b2) -> unifyOne pos (ConsEq b1 b2)
            _                  -> return []



unify :: BoM UnifyState m => [(Constraint, TextPos)] -> m [(Type, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (snd x) $ applySubs subs (fst x)
    return (s ++ subs)


unifyDefault :: BoM UnifyState m => [(Constraint, TextPos)] -> m [(Type, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    -- ignore errors in default mode
    s <- catchError (unifyOne (snd x) (applySubs subs (fst x))) (\_ -> return []) 
    return (s ++ subs)

