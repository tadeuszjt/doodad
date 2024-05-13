module Unify where

import Control.Monad.Except

import Type
import ASTResolved
import Constraint
import Monad
import Error
import Apply
import Symbol


unifyOne :: ConstraintInfo -> Constraint -> DoM ASTResolved [(Type, Type)]
unifyOne info constraint = withPos info $ case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2             -> return []
        (Type x, t)              -> return [(Type x, t)]
        (t, Type x)              -> return [(Type x, t)]
        (Slice a, Slice b)       -> unifyOne info (ConsEq a b)
        (TypeApply s1 ts1, TypeApply s2 ts2)
            | length ts1 == length ts2 && s1 == s2 ->
                concat <$> zipWithM (\a b -> unifyOne info (ConsEq a b)) ts1 ts2

        _ -> fail $ (infoMsg info) ++ ":" ++ show (t1, t2)

    ConsField typ idx exprType -> do
        base <- baseTypeOfm typ
        case base of
            Nothing                             -> return []
            Just (TypeApply (Sym ["Tuple"]) ts) -> unifyOne info $ ConsEq exprType (ts !! idx)
            Just (TypeApply (Sym ["Sum"]) ts)   -> unifyOne info $ ConsEq exprType (ts !! idx)
            x                                   -> fail ("cannot take field")

    ConsSlice exprType typ -> do
        basem <- baseTypeOfm typ
        case basem of
            Nothing -> return []
            Just (TypeApply (Sym ["Table"]) [t]) -> unifyOne info $ ConsEq exprType (Type.Slice t)
            x -> error (show x)

    ConsDefault t1 t2 -> case (t1, t2) of
        _ | t1 == t2 -> return []
        (Type _, _)  -> return [(t1, t2)]
        (_, _)       -> return []

    x -> error "invalid constraint"


unify :: [(Constraint, ConstraintInfo)] -> DoM ASTResolved [(Type, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (snd x) (applyConstraint subs (fst x))
    return (s ++ subs)
