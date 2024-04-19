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
import FunctionFinder (findCandidates)


unifyOne :: TextPos -> Constraint -> DoM ASTResolved [(Type, Type)]
unifyOne pos constraint = withPos pos $ case constraint of
    ConsCall exprType symbol argTypes -> do
        ast <- get
        candidates <- if symbolIsResolved symbol then
                return [symbol]
            else
                fmap fst $ runDoMExcept ast $ findCandidates (CallHeader symbol argTypes exprType)
        case candidates of
            [symbol] | isGenericFunction symbol ast -> return []
            [symbol] | isNonGenericFunction symbol ast -> do
                let body = getFunctionBody symbol ast

                subs <- unifyOne pos $ ConsEq exprType (typeof $ funcRetty body)
                subs' <- fmap concat $ zipWithM (\a b -> unifyOne pos $ ConsEq a b) argTypes
                    (map typeof $ funcArgs body)
                return (subs ++ subs')

            _ -> return []


    ConsField typ idx exprType -> do
        base <- baseTypeOfm typ
        case base of
            Nothing         -> return []
            Just (Tuple ts) -> unifyOne pos $ ConsEq exprType (ts !! idx)
            Just (Table t)  -> do
                baseT <- baseTypeOfm t
                case baseT of
                    _ -> error ""
            x -> error (show x)


    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2             -> return []
        (Type x, t)              -> return [(Type x, t)]
        (t, Type x)              -> return [(Type x, t)]
        (Table t1, Table t2)     -> unifyOne pos (ConsEq t1 t2)
        (Tuple ts1, Tuple ts2)
            | length ts1 == length ts2 -> concat <$> zipWithM (\a b -> unifyOne pos (ConsEq a b)) ts1 ts2

        _ -> fail ("type mismatch: " ++ show t1 ++ " != " ++ show t2)


    ConsAdtField adtType i ts -> do
        basem <- baseTypeOfm adtType
        case basem of
            Nothing -> return []
            Just (Sum ts') -> do
                unless (i >= 0 && i < length ts') (error "index out of range")
                case ts of
                    []  -> unifyOne pos $ ConsEq Void (ts' !! i)
                    [t] -> unifyOne pos $ ConsEq t (ts' !! i)

    ConsForExpr t1 t2 -> do
        basem <- baseTypeOfm t1
        case basem of
            Nothing -> return []

            x -> error (show x)

    ConsBase t1 t2 -> do
        base1m <- baseTypeOfm t1
        base2m <- baseTypeOfm t2
        case (base1m, base2m) of
            (Just b1, Just b2) -> unifyOne pos (ConsEq b1 b2)
            _                  -> return []


unify :: [(Constraint, TextPos)] -> DoM ASTResolved [(Type, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (snd x) (applyConstraint subs (fst x))
    return (s ++ subs)


unifyDefault :: [(Constraint, TextPos)] -> DoM ASTResolved [(Type, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    -- ignore errors in default mode
    res <- tryError $ unifyOne (snd x) (applyConstraint subs (fst x))
    case res of
        Left _  -> return subs
        Right s -> return (s ++ subs)

