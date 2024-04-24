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


-- Function Finder contains functions which can determine whether a generic function could match
-- a function call. This involves using a smaller version of the type inference algorithm to replace
-- generic symbols with resolved types.
findCandidates :: CallHeader -> DoM ASTResolved [CallHeader]
findCandidates call = do
    ast <- get
    --liftIO $ putStrLn $ "findCandidates: " ++ show (callSymbol call)
    fmap catMaybes $ forM (Map.toList $ Map.union (funcDefs ast) (funcImports ast)) $
        \(symbol, body) -> do
            couldMatch <- callCouldMatchFunc call symbol body
            if couldMatch then case isGenericFunction symbol ast of
                False -> return $ Just $ getFunctionCallHeader symbol ast
                True -> do
                    replacedE <- tryError (replaceGenericsInFuncBodyWithCall body call)
                    case replacedE of
                        Left _ -> return Nothing
                        Right replaced -> do
                            b' <- callCouldMatchFunc call symbol replaced
                            if b' then return $ Just $ CallHeader
                                symbol
                                (map typeof $ funcArgs replaced)
                                (typeof $ funcRetty replaced)

                            else return Nothing
                        x -> error (show x)
            else return Nothing



callCouldMatchFunc :: TypeDefs m => CallHeader -> Symbol -> FuncBody -> m Bool
callCouldMatchFunc call symbol body = do
    if symbolsMatch then do
        am <- argsMatch
        rm <- rettyMatch
        return (am && rm)
    else return False
    where
        typesMatch :: TypeDefs m => [Type] -> [Type] -> m Bool
        typesMatch ts1 ts2 = do
            bs <- zipWithM (typesCouldMatch (funcGenerics body)) ts1 ts2
            return $ (length ts1 == length ts2) && (all id bs)

        symbolsMatch    = symbolsCouldMatch (callSymbol call) symbol
        argsMatch       = typesMatch (callArgTypes call) (map typeof $ funcArgs body)
        rettyMatch      = typesCouldMatch (funcGenerics body) (callRetType call) (typeof $ funcRetty body)


funcFullyResolved :: [Symbol] -> FuncBody -> Bool
funcFullyResolved generics body =
    all (typeFullyResolved generics) (map typeof $ funcArgs body) &&
    (typeFullyResolved generics) (typeof (funcRetty body))


replaceGenericsInFuncBodyWithCall :: (MonadFail m, TypeDefs m) => FuncBody -> CallHeader -> m FuncBody
replaceGenericsInFuncBodyWithCall body call = do
    couldMatch <- callCouldMatchFunc call (callSymbol call) body
    unless couldMatch (error "headers could not match")
    subs <- unify (funcGenerics body) =<< getConstraints call body
    return $ (applyFuncBody subs body) { funcGenerics = [] }


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


getConstraints :: (TypeDefs m, MonadFail m) => CallHeader -> FuncBody -> m [Constraint]
getConstraints call body = do
    retCs <- getConstraintsFromTypes (funcGenerics body) (typeof $ funcRetty body) (callRetType call)
    argCs <- fmap concat $ zipWithM (getConstraintsFromTypes $ funcGenerics body)
        (map typeof $ funcArgs body)
        (callArgTypes call)
    return (retCs ++ argCs)
    


getConstraintsFromTypes :: (TypeDefs m, MonadFail m) => [Symbol] -> Type -> Type -> m [Constraint]
getConstraintsFromTypes generics t1 t2 = fromTypes t1 t2
    where
        fromTypes :: (TypeDefs m, MonadFail m) => Type -> Type -> m [Constraint]
        fromTypes t1 t2 = do
            typedefs <- getTypeDefs
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
