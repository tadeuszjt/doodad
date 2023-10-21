{-# LANGUAGE FlexibleContexts #-}
module TypeConstraints where

import Control.Monad.State

import Symbol
import Type
import Constraint
import ASTResolved 
import Monad
import Apply


unifyOne :: BoM s m => [Symbol] -> Constraint -> m [(Type, Type)]
unifyOne typeVars constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                            -> return []
        (TypeApply s [], _) | s `elem` typeVars -> return [(t1, t2)]
        (Type _, _)                             -> return [(t1, t2)]
        (_, Type _)                             -> return [(t2, t1)]

        (Record _, Tuple _) -> fail $ "cannot unify: " ++ show t1 ++ " with: " ++ show t2
        _ -> error $ show (t1, t2)


unify :: BoM s m => [Symbol] -> [Constraint] -> m [(Type, Type)]
unify typeVars []     = return []
unify typeVars (x:xs) = do
    subs <- unify typeVars xs
    s <- unifyOne typeVars =<< applySubs subs x
    return (s ++ subs)


getConstraintsFromTypes :: BoM TypeFuncs m => [Symbol] -> Type -> Type -> m [Constraint]
getConstraintsFromTypes typeArgs t1 t2 = do
    typeFuncs <- get
    case (flattenTuple typeFuncs t1, flattenTuple typeFuncs t2) of
        (a, b) | a == b          -> return []
        (Type a, Type b)         -> return [ConsEq (Type a) (Type b)]
        (Type a, b) | isSimple b -> return [ConsEq (Type a) b]

        (a, b) -> error $ show (a, b)
