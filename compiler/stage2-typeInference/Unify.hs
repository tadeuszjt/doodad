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



allSameType :: [Type] -> Maybe Type
allSameType [] = Nothing
allSameType [x] = Just x
allSameType (x:xs) = case allSameType xs of
    Just t -> if x == t then Just t else Nothing
    Nothing -> Nothing



unifyOne :: ConstraintInfo -> Constraint -> DoM ASTResolved [(Type, Type)]
unifyOne info constraint = withPos info $ case constraint of
    ConsCall exprType symbol argTypes -> do
        candidates <- findCandidates (CallHeader symbol argTypes exprType)

        subs1 <- case allSameType (map callRetType candidates) of
            Just x | typeFullyResolved x -> unifyOne info $ ConsEq exprType x
            _ -> return []

        subs2 <- fmap concat $ forM (zip [0..] argTypes) $ \(i, at) -> do
            case allSameType (map ((!! i) . callArgTypes) candidates) of
                Just x | typeFullyResolved x -> unifyOne info $ ConsEq at x
                _ -> return []

        return (subs1 ++ subs2)


    ConsField typ idx exprType -> do
        base <- baseTypeOfm typ
        case base of
            Nothing         -> return []
            Just (TypeApply (Sym "Tuple") ts) -> unifyOne info $ ConsEq exprType (ts !! idx)
            Just (TypeApply (Sym "Sum") ts)   -> unifyOne info $ ConsEq exprType (ts !! idx)
            x -> error (show x)


    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2             -> return []
        (Type x, t)              -> return [(Type x, t)]
        (t, Type x)              -> return [(Type x, t)]
        (Slice a, Slice b)       -> unifyOne info $ ConsEq a b
        (TypeApply s1 ts1, TypeApply s2 ts2)
            | length ts1 == length ts2 ->
                concat <$> zipWithM (\a b -> unifyOne info (ConsEq a b)) ts1 ts2

        _ -> fail $ (infoMsg info) ++ ":" ++ show (t1, t2)


    ConsPatField patType symbol argType -> do
        error "here"
        

    ConsPatTypeField patType fieldType argTypes -> case argTypes of
        [] -> return []
        [argType] -> unifyOne info (ConsEq fieldType argType)


    ConsForExpr exprType patType -> do
        basem <- baseTypeOfm exprType
        case basem of
            Nothing -> return []
            Just (TypeApply (Sym "Table") [t])         -> unifyOne info $ ConsEq patType t
            Just (TypeApply (Sym "Array") [t, Size n]) -> unifyOne info $ ConsEq patType t
            Just (TypeApply (Sym "Tuple") [t1, t2])    -> do
                unifyOne info $ ConsEq t1 t2
                unifyOne info $ ConsEq patType t1
            Just (Slice t) -> unifyOne info $ ConsEq patType t

            x -> fail (show x)

    ConsBase t1 t2 -> do
        base1m <- baseTypeOfm t1
        base2m <- baseTypeOfm t2
        case (base1m, base2m) of
            (Just b1, Just b2) -> unifyOne info (ConsEq b1 b2)
            _                  -> return []

    ConsSlice exprType typ -> do
        basem <- baseTypeOfm typ
        case basem of
            Nothing -> return []
            Just (TypeApply (Sym "Table") [t]) -> unifyOne info $ ConsEq exprType (Type.Slice t)
            x -> error (show x)

    ConsDefault t1 t2 -> case (t1, t2) of
        _ | t1 == t2 -> return []
        (Type _, _)              -> return [(t1, t2)]
        (x, _) | isSimple x      -> return []

        (TypeApply (Sym "Tuple") ts1, TypeApply (Sym "Tuple") ts2)
            | length ts1 == length ts2 -> fmap concat $
                zipWithM (\a b -> unifyOne info (ConsDefault a b)) ts1 ts2

        (TypeApply (Sym "Table") ts1, TypeApply (Sym "Table") ts2)
            | length ts1 == length ts2 -> fmap concat $
                zipWithM (\a b -> unifyOne info (ConsDefault a b)) ts1 ts2

        (TypeApply (SymResolved _ _ _) _, _) -> return []

        x -> error (show x)


    x -> error (show x)


unify :: [(Constraint, ConstraintInfo)] -> DoM ASTResolved [(Type, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (snd x) (applyConstraint subs (fst x))
    return (s ++ subs)


unifyDefault :: [(Constraint, ConstraintInfo)] -> DoM ASTResolved [(Type, Type)]
unifyDefault []     = return []
unifyDefault (x:xs) = do
    subs <- unifyDefault xs
    s <- unifyOne (snd x) (applyConstraint subs (fst x))
    return (s ++ subs)

