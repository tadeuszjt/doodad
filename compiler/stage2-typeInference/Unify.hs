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



unifyOne :: TextPos -> Constraint -> DoM ASTResolved [(Type, Type)]
unifyOne pos constraint = withPos pos $ case constraint of
    ConsIdent t1 t2 -> do
        base <- baseTypeOfm t1
        case base of
            Nothing -> return []
            Just _                  -> unifyOne pos (ConsEq t2 t1)


    ConsField typ (Sym _) exprType -> return []

    ConsField typ symbol exprType -> do
        resm <- Map.lookup symbol <$> gets ctorDefs
        base <- baseTypeOfm typ
        case base of
            Just (Tuple ts) -> case resm of
                Just (typeSymbol, i) -> unifyOne pos $ ConsEq exprType (ts !! i) --TODO, check more?

            Just (Table t)   -> do
                baseT <- baseTypeOfm t
                case baseT of
                    _ -> error ""

    ConsTuple tupType ts1 -> do
        basem <- baseTypeOfm tupType
        case basem of
            Nothing -> return []
            Just (Tuple ts2)
                | length ts1 == length ts2 -> concat <$> zipWithM (\a b -> unifyOne pos (ConsEq a b)) ts1 ts2

            _ -> error (show basem)

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
            Just (ADT ts') -> do
                unless (i >= 0 && i < length ts') (error "index out of range")
                case ts of
                    []  -> unifyOne pos $ ConsEq Void (ts' !! i)
                    [t] -> unifyOne pos $ ConsEq t (ts' !! i)
                    ts  -> unifyOne pos $ ConsTuple (ts' !! i) ts

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

    ConsBuiltinAt t1 t2 -> do
        baseT1m <- baseTypeOfm t1
        case baseT1m of
            Nothing -> return []
            Just x | isSimple x -> return []
            x -> error (show x)



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

