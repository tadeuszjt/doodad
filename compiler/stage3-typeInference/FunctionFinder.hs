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


findInstance :: MonadIO m => ASTResolved -> CallHeader -> m (Maybe Symbol)
findInstance ast call = do
    let funcs = Map.unions [funcInstance ast, funcInstanceImported ast]

    candidates <- fmap catMaybes $ forM (Map.toList funcs) $ \(header, func) -> do
        let match = (symbolsCouldMatch (callSymbol call) (callSymbol header)) &&
                    (callRetType call == (callRetType header)) &&
                    (callArgTypes call == (callArgTypes header))
        case match of
            True -> return (Just $ funcSymbol $ funcHeader func)
            False -> return Nothing
    case candidates of
        [] -> return Nothing
        [x] -> do
            return (Just x)
        xs -> error ("multiple candidates for: " ++ show call)


findCandidates :: (MonadFail m, MonadError Error m) => CallHeader -> [FuncHeader] -> m [FuncHeader]
findCandidates call headers = do
    fmap catMaybes $ forM headers $ \header -> do
        couldMatch <- callCouldMatchFunc call header
        if not couldMatch then
            return Nothing
        else if not (isGenericHeader header) then
            return (Just header)
        else do
            replacedE <- tryError (replaceGenericsInFuncHeader header call)
            case replacedE of
                Left _ -> return Nothing
                Right replaced -> do
                    res <- callCouldMatchFunc call replaced
                    unless res (error "call doesn't match after replace")
                    return (Just replaced)



replaceGenericsInFuncHeader :: MonadFail m => FuncHeader -> CallHeader -> m FuncHeader
replaceGenericsInFuncHeader header call = do
    couldMatch <- callCouldMatchFunc call (header { funcSymbol = callSymbol call })
    unless couldMatch (error "headers could not match")
    subs <- unify (funcGenerics header) =<< getConstraints call header
    return (applyFuncHeader subs header)


replaceGenericsInFuncWithCall :: MonadFail m => Func -> CallHeader -> m Func
replaceGenericsInFuncWithCall func call = do
    couldMatch <- callCouldMatchFunc call $ (funcHeader func) { funcSymbol = callSymbol call }
    unless couldMatch (error "headers could not match")
    subs <- unify (funcGenerics $ funcHeader func) =<< getConstraints call (funcHeader func)
    let func'   =  applyFunc subs func
    let header' = (funcHeader func') { funcGenerics = [] }
    return $ func' { funcHeader = header' } 


unifyOne :: MonadFail m => [Symbol] -> Constraint -> m [(Type, Type)]
unifyOne generics constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                            -> return []
        (TypeApply s [], _) | s `elem` generics -> return [(t1, t2)]
        (Type _, _)                             -> return [(t1, t2)]
        (Slice a, Slice b)                      -> return [(t1, t2)]
        (TypeApply s1 ts1, TypeApply s2 ts2)
            | length ts1 == length ts2 ->
                concat <$> zipWithM (\a -> unifyOne generics . ConsEq a) ts1 ts2

        _ -> fail $ "cannot unify: " ++ show (t1, t2)


unify :: MonadFail m => [Symbol] -> [Constraint] -> m [(Type, Type)]
unify generics []     = return []
unify generics (x:xs) = do
    subs <- unify generics xs
    s <- unifyOne generics (applyConstraint subs x)
    return (s ++ subs)


getConstraints :: MonadFail m => CallHeader -> FuncHeader -> m [Constraint]
getConstraints call header = do
    retCs <- getConstraintsFromTypes
        (funcGenerics header)
        (typeof $ funcRetty header)
        (callRetType call)
    argCs <- fmap concat $ zipWithM (getConstraintsFromTypes $ funcGenerics header)
        (map typeof $ funcArgs header)
        (callArgTypes call)
    return (retCs ++ argCs)
    


getConstraintsFromTypes :: MonadFail m => [Symbol] -> Type -> Type -> m [Constraint]
getConstraintsFromTypes generics t1 t2 = fromTypes t1 t2
    where
        fromTypes :: MonadFail m => Type -> Type -> m [Constraint]
        fromTypes t1 t2 = do
            case (t1, t2) of
                (a, b) | a == b            -> return [] 
                (Type _, _)                -> return [ConsEq t1 t2]
                (_, Type _)                -> return []
                (Slice a, Slice b)         -> return [ConsEq a b]

                (TypeApply s1 ts1, TypeApply s2 ts2)
                    | s1 `elem` generics -> do 
                        unless (not $ s2 `elem` generics) (error "unknown")

                        case ts1 of
                            [] -> return [ConsEq t1 t2]
                            _  -> do
                                unless (length ts1 == length ts2) (error "type mismatch")
                                (ConsEq t1 t2 :) . concat <$> zipWithM fromTypes ts1 ts2

                    | not (elem s1 generics) && s1 == s2 -> do
                        unless (length ts1 == length ts2) (error "type mismatch")
                        (ConsEq t1 t2 :) . concat <$> zipWithM fromTypes ts1 ts2

                    | otherwise -> fail "type mismatch"

                (TypeApply s1 [], t) | elem s1 generics -> return [ConsEq t1 t2]

                _ -> fail $ show (t1, t2)