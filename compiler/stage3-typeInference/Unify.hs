module Unify where

import Type
import ASTResolved
import Constraint
import Monad
import Error
import Apply


unifyOne :: ConstraintInfo -> Constraint -> DoM ASTResolved [(Type, Type)]
unifyOne info constraint = withPos info $ case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2             -> return []
        (Type x, t)              -> return [(Type x, t)]
        (t, Type x)              -> return [(Type x, t)]

        (Apply a1 a2, Apply b1 b2) -> do
            subs1 <- unifyOne info (ConsEq a1 b1)
            subs2 <- unifyOne info (ConsEq a2 b2)
            return (subs1 ++ subs2)
            
        _ -> fail $ (infoMsg info) ++ ":" ++ show (t1, t2)

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
