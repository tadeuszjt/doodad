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
import ASTResolved
import qualified AST as S
import Constraint
import Monad
import Error
import Apply
import Symbol


baseTypeOf :: BoM ASTResolved m => Type -> m (Maybe Type)
baseTypeOf typ = case typ of
    TypeApply symbol ts -> do
        resm <- gets $ Map.lookup symbol . typeFuncs
        case resm of
            Nothing              -> return Nothing
            Just (argSymbols, t) -> do
                assert (length argSymbols == length ts) "invalid number of type arguments"
                baseTypeOf $ applyTypeArguments argSymbols ts t
    Type x -> return Nothing
    t      -> return (Just t)


unifyOne :: BoM ASTResolved m => TextPos -> Constraint -> m [(Type, Type)]
unifyOne pos constraint = withPos pos $ case constraint of
    ConsField typ (Sym _) exprType -> return []

    ConsField typ symbol exprType -> do
        resm <- Map.lookup symbol <$> gets ctorDefs
        base <- baseTypeOf typ
        case base of
            Just (Record ts) -> case resm of
                Just (typeSymbol, index) -> unifyOne pos $ ConsEq (ts !! index) exprType
                Nothing                  -> case elemIndex (TypeApply symbol []) ts of
                    Just index -> unifyOne pos $ ConsEq (ts !! index) exprType
                    x          -> error (show x)

            Just (Tuple t)   -> do
                baseT <- baseTypeOf t
                case baseT of
                    Just (Record ts) -> case resm of
                        Just (typeSymbol, index) -> unifyOne pos $ ConsEq (ts !! index) exprType
                        Nothing                  -> case elemIndex (TypeApply symbol []) ts of
                            Just index -> unifyOne pos $ ConsEq (ts !! index) exprType
                            x          -> error (show x)

            Just (Table t)   -> do
                baseT <- baseTypeOf t
                case baseT of
                    Just (Record ts) -> case resm of
                        Just (typeSymbol, index) -> unifyOne pos $ ConsEq (Table $ ts !! index) exprType


    ConsTuple tupType ts -> do
        basem <- baseTypeOf tupType
        case basem of
            Nothing -> return []
            Just (Tuple t) -> do
                baseT <- baseTypeOf t
                case baseT of
                    Just (Record _) -> do
                        typeDefs <- gets typeFuncs
                        let recordTs = getRecordTypes typeDefs t
                        assert (length ts == length recordTs) "record length mismatch"
                        concat <$> zipWithM (\a b -> unifyOne pos $ ConsEq a b) ts recordTs
                    _ -> error (show baseT)

    ConsRecordAccess exprType typ -> do
        base <- baseTypeOf typ
        case base of
            Just (Tuple t) -> do
                baseT <- baseTypeOf t
                case baseT of
                    Just (Record ts) -> unifyOne pos $ ConsEq exprType t
                    _ -> error (show baseT)

            Just t | isSimple t -> unifyOne pos $ ConsEq exprType (Record [t])
            Nothing -> return []
            _ -> error (show base)

    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                    -> return []
        (Type x, t)                     -> return [(Type x, t)]
        (t, Type x)                     -> return [(Type x, t)]
        (Record ts1, Record ts2) -> do
            assert (length ts1 == length ts2) $ "record length mismatch: " ++ show (t1, t2)
            unify $ zipWith (\a b -> (ConsEq a b, pos)) ts1 ts2
        (Table t1, Table t2)      -> unifyOne pos $ ConsEq t1 t2
        (Tuple t1, Tuple t2)      -> unifyOne pos $ ConsEq t1 t2

        _ -> fail $ "un - cannot unify " ++ show t1 ++ " with " ++ show t2

    ConsAdtField t i j adt -> do
        basem <- baseTypeOf adt
        case basem of
            Nothing -> return []
            Just (ADT ts) -> do
                assert (i < length ts) "Invalid ADT field index"
                assert (j == 0)        "Invalid ConsAdtField"
                unifyOne pos $ ConsEq t (ts !! i)
            _ -> error (show basem)
        

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
            Just (Range _) -> unifyOne pos (ConsEq t2 I64)
            Just String    -> unifyOne pos (ConsEq t2 Char)
            Just (Table t) -> do
                baseT <- baseTypeOf t
                case baseT of
                    Nothing -> return []
                    Just (Record _) -> unifyOne pos (ConsEq t2 t)
                    Just (Tuple t)  -> unifyOne pos (ConsEq t2 t)
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



unify :: BoM ASTResolved m => [(Constraint, TextPos)] -> m [(Type, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (snd x) =<< applySubs subs (fst x)
    return (s ++ subs)


unifyDefault :: BoM ASTResolved m => [(Constraint, TextPos)] -> m [(Type, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    -- ignore errors in default mode
    res <- tryError $ unifyOne (snd x) =<< applySubs subs (fst x)
    case res of
        Left _  -> return subs
        Right s -> return (s ++ subs)

