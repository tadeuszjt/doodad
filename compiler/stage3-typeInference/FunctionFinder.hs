{-# LANGUAGE FlexibleContexts #-}
module FunctionFinder where

import Data.Maybe
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.Except

import Control.Monad.Fail
import Control.Monad.IO.Class

import ASTResolved
import Symbol
import Type
import Apply
import Error
import Constraint
import Monad
import AST


-- Function Finder contains functions which can determine whether a generic function could match
-- a function call. This involves using a smaller version of the type inference algorithm to replace
-- generic symbols with resolved types.


findInstance :: MonadIO m => ASTResolved -> Symbol -> Type -> m (Maybe Symbol)
findInstance ast symbol callType = do
    let funcs = Map.unions [funcInstance ast, funcInstanceImported ast]

    candidates <- fmap catMaybes $ forM (Map.toList funcs) $ \((instSymbol, instType), func) -> do
        let match = symbolsCouldMatch symbol instSymbol && callType == instType
        case match of
            True -> return (Just $ funcSymbol $ funcHeader func)
            False -> return Nothing
    case candidates of
        [] -> return Nothing
        [x] -> do
            return (Just x)
        xs -> error ("multiple candidates for: " ++ show callType)


findCandidates :: (MonadFail m, MonadError Error m) => Type -> [FuncHeader] -> m [FuncHeader]
findCandidates callType headers = do
    fmap catMaybes $ forM headers $ \header -> do
        if not (typesCouldMatch callType $ typeof header) then
            return Nothing
        else do
            replacedE <- tryError $ replaceGenericsInFuncHeader header callType
            case replacedE of
                Left _ -> return Nothing
                Right replaced -> do
                    let res = typesCouldMatch callType (typeof replaced)
                    unless res (error "call doesn't match after replace")
                    return (Just replaced)


replaceGenericsInType :: MonadFail m => Type -> Type -> m Type
replaceGenericsInType t1 t2 = do
    subs <- unify =<< getConstraintsFromTypes t1 t2
    return (applyType subs t1)


replaceGenericsInFuncHeader :: MonadFail m => FuncHeader -> Type -> m FuncHeader
replaceGenericsInFuncHeader header callType = do
    let couldMatch = typesCouldMatch callType (typeof header)
    unless couldMatch (error "headers could not match")
    subs <- unify =<< getConstraintsFromTypes (typeof header) callType
    return (applyFuncHeader subs header)


replaceGenericsInFunc :: MonadFail m => Func -> Type -> m Func
replaceGenericsInFunc func callType = do
    let couldMatch = typesCouldMatch callType (typeof $ funcHeader func)
    unless couldMatch (error "headers could not match")
    subs <- unify =<< getConstraintsFromTypes (typeof $ funcHeader func) callType
    return (applyFunc subs func)


unifyOne :: MonadFail m => Constraint -> m [(Type, Type)]
unifyOne constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2       -> return []
        _ | isGeneric t1   -> return [(t1, t2)]
        (Type _, _)        -> return [(t1, t2)]

        (Apply Slice [t1], Apply Slice [t2]) -> return [(t1, t2)]

        (TypeDef _, _) -> error "here"
        (Apply (TypeDef _) _, _) -> error $ show (t1, t2)

        _ -> fail $ "cannot unify: " ++ show (t1, t2)


unify :: MonadFail m => [Constraint] -> m [(Type, Type)]
unify []     = return []
unify (x:xs) = do
    subs <- unify xs
    s <- unifyOne (applyConstraint subs x)
    return (s ++ subs)


getConstraintsFromTypes :: MonadFail m => Type -> Type -> m [Constraint]
getConstraintsFromTypes t1 t2 = case (t1, t2) of
    (a, b) | a == b            -> return [] 
    (Type _, _)                -> return [ConsEq t1 t2]
    (_, Type _)                -> return []

    (Apply t1 ts1, Apply t2 ts2) -> do
        unless (length ts1 == length ts2) (error "type mismatch")
        concat <$> zipWithM (getConstraintsFromTypes) (t1 : ts1) (t2 : ts2)

    _ | isGeneric t1 && isGeneric t2 -> error $ show (t1, t2)
    _ | isGeneric t1 && not (isGeneric t2) -> return [ConsEq t1 t2]

    x -> error (show x)
    _ -> fail $ show (t1, t2)
