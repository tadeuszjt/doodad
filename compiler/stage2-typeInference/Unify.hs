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


type UnifyState = Map.Map Symbol ([Symbol], Type)


baseTypeOf :: BoM UnifyState m => Type -> m (Maybe Type)
baseTypeOf typ = case typ of
    TypeApply symbol ts -> do
        resm <- gets $ Map.lookup symbol
        case resm of
            Nothing              -> return Nothing
            Just (argSymbols, t) -> do
                assert (length argSymbols == length ts) "invalid number of type arguments"
                baseTypeOf $ applyTypeFunction argSymbols ts t
    Type x -> return Nothing
    t      -> return (Just t)



unifyOne :: BoM UnifyState m => TextPos -> Constraint -> m [(Type, Type)]
unifyOne pos constraint = withPos pos $ case constraint of
    ConsTuple tupType ts -> do
        basem <- baseTypeOf tupType
        case basem of
            Nothing -> return []
            Just (Tuple t) -> do
                baseT <- baseTypeOf t
                case baseT of
                    Just (Record _) -> unifyOne pos $ ConsBase t (Record ts)
                    _ -> error (show baseT)

    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                    -> return []
        (Type x, t)                     -> return [(Type x, t)]
        (t, Type x)                     -> return [(Type x, t)]
        (Record ts1, Record ts2) -> do
            assert (length ts1 == length ts2) "record length mismatch"
            unify $ zipWith (\a b -> (ConsEq a b, pos)) ts1 ts2
        (Table t1, Table t2)      -> unifyOne pos $ ConsEq t1 t2
        (Tuple t1, Tuple t2)      -> unifyOne pos $ ConsEq t1 t2

        _ -> fail $ "cannot unify " ++ show t1 ++ " with " ++ show t2

    ConsAdtMem t i j agg -> do
        basem <- baseTypeOf agg
        case basem of
--            Just (ADT fs) -> do
--                assert (i < length fs) "Invalid ADT member"
--                case fs !! i of
--                    FieldNull -> fail "Invalid ADT member"
--                    FieldType ft -> do
--                        assert (j == 0) "Invalid ADT member"
--                        unifyOne pos $ ConsEq t ft
--                    FieldCtor ts -> do
--                        assert (j < length ts) "Invalid ADT member"
--                        unifyOne pos $ ConsEq t (ts !! j)
            _ -> error (show basem)
        
    ConsField t i agg -> do
        basem <- baseTypeOf agg
        case basem of
            Just (Tuple (Record ts)) -> unifyOne pos (ConsEq t $ ts !! i)
            _ -> error (show basem)
            _ -> return []

    ConsMember t1 i t2 -> do
        basem <- baseTypeOf t1
        case basem of
--            Just (Tuple ts)  -> unifyOne pos (ConsEq t2 $ ts !! i)
--            Just (Table ts)  -> unifyOne pos (ConsEq t2 $ ts !! i)
--            Just (Array n t) -> do 
--                assert (i == 0) "ConsMember: Invalid index"
--                unifyOne pos (ConsEq t2 t)
--            Just (Range t)   -> do 
--                assert (i == 0) "ConsMember: Invalid index"
--                unifyOne pos (ConsEq t2 t)
            Just String -> do
                unifyOne pos (ConsEq t2 Char)
            Nothing -> return []
            _ -> error (show basem)

    ConsElem t1 t2 -> do
        basem <- baseTypeOf t1
        case basem of
--            Just (Table [t]) -> unifyOne pos (ConsEq t2 t)
--            Just (Table ts)  -> unifyOne pos (ConsEq t2 $ Tuple ts)
--            Just (Array n t) -> unifyOne pos (ConsEq t2 t)
            Nothing -> return []
            _ -> error (show basem)

    ConsSubscript t1 t2 -> do
        basem <- baseTypeOf t1
        case basem of
--            Just (Table [t])  -> unifyOne pos (ConsEq t2 t)
--            Just (Table ts)   -> unifyOne pos (ConsEq t2 $ Tuple ts)
--            Just (Array n t)  -> unifyOne pos (ConsEq t2 t)
            Just String    -> unifyOne pos (ConsEq t2 Char)
            Just (Tuple t) -> do
                baseT <- baseTypeOf t
                case baseT of
                    Nothing -> return []
                    Just (Record _) -> unifyOne pos (ConsEq t2 t)
                    Just _          -> unifyOne pos (ConsEq t2 $ Record [t])
            Just (Table t) -> do
                baseT <- baseTypeOf t
                case baseT of
                    Nothing -> return []
                    Just (Record _) -> unifyOne pos (ConsEq t2 t)
                    Just _          -> unifyOne pos (ConsEq t2 $ Record [t])

            Nothing -> return []
            _ -> error (show basem)
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
    s <- unifyOne (snd x) =<< applySubs subs (fst x)
    return (s ++ subs)


unifyDefault :: BoM UnifyState m => [(Constraint, TextPos)] -> m [(Type, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    -- ignore errors in default mode
    res <- tryError $ unifyOne (snd x) =<< applySubs subs (fst x)
    case res of
        Left _  -> return subs
        Right s -> return (s ++ subs)

