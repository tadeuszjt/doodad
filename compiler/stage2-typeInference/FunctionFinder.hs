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
import ASTMapper
import TupleDeleter


-- Function Finder contains functions which can determine whether a generic function could match
-- a function call. This involves using a smaller version of the type inference algorithm to replace
-- generic symbols with resolved types.
findCandidates :: CallHeader -> DoM ASTResolved [Symbol]
findCandidates call = do
    ast <- get
    --liftIO $ putStrLn $ "findCandidates: " ++ show (callSymbol call)
    fmap catMaybes $ forM (Map.toList $ Map.union (funcDefs ast) (funcImports ast)) $
        \(symbol, body) -> do
            b <- callCouldMatchFunc call symbol body
            return $ case b of
                True -> Just symbol
                False -> Nothing


callCouldMatchFunc :: CallHeader -> Symbol -> FuncBody -> DoM ASTResolved Bool
callCouldMatchFunc call symbol body = do
    if symbolsMatch then do
        am <- argsMatch
        rm <- rettyMatch
        pm <- paramMatches
        return (am && rm && pm)
    else return False
    where
        typesMatch :: [Type] -> [Type] -> DoM ASTResolved Bool
        typesMatch ts1 ts2 = do
            bs <- zipWithM (typesCouldMatch (funcGenerics body)) ts1 ts2
            return $ length ts1 == length ts2 && (all id bs)

        symbolsMatch    = symbolsCouldMatch (callSymbol call) symbol
        argsMatch       = typesMatch (callArgTypes call) (map typeof $ funcArgs body)
        rettyMatch      = typesMatch [callRetType call] [funcRetty body]

        paramMatches = case (callParamType call, map typeof (funcParams body)) of
            (Nothing, xs)       -> return (xs == [])
            (Just (Type _), xs) -> return (xs /= [])
            (Just t1,  ts2)     -> do
                baseT1 <- baseTypeOf t1
                typesMatch ts2 =<< case baseT1 of 
                    Record xs -> return xs
                    t         -> return [t1]


funcFullyResolved :: [Symbol] -> FuncBody -> Bool
funcFullyResolved generics body =
    all id (map typeFullyResolved $ map typeof $ funcParams body) &&
    all id (map typeFullyResolved $ map typeof $ funcArgs body) &&
    typeFullyResolved (funcRetty body)
    where
        typeFullyResolved :: Type -> Bool
        typeFullyResolved typ = case typ of
            Type _                          -> False
            TypeApply s _ | elem s generics -> False
            TypeApply s ts                  -> all id (map typeFullyResolved ts)
            Record ts                       -> all id (map typeFullyResolved ts)
            Table t                         -> typeFullyResolved t
            Tuple t                         -> typeFullyResolved t
            RecordApply t                   -> typeFullyResolved t
            x | isSimple x                  -> True
            Void                            -> True
            x -> error $ "typeFullyResolved: " ++ show x


replaceGenericsInFuncBodyWithCall :: FuncBody -> CallHeader -> DoM ASTResolved FuncBody
replaceGenericsInFuncBodyWithCall body call = do
    couldMatch <- callCouldMatchFunc call (callSymbol call) body
    unless couldMatch (error "headers could not match")
    subs <- unify (funcGenerics body) =<< getConstraints call body
    mapFuncBodyM tupleDeleterMapper $ (applyFuncBody subs body) { funcGenerics = [] }


unifyOne :: [Symbol] -> Constraint -> DoM ASTResolved [(Type, Type)]
unifyOne generics constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                            -> return []
        (TypeApply s [], _) | s `elem` generics -> return [(t1, t2)]
        (Type _, _)                             -> return [(t1, t2)]
        (Tuple a, Tuple b)                      -> unifyOne generics $ ConsEq a b
        (TypeApply s1 ts1, TypeApply s2 ts2)
            | length ts1 == length ts2 ->
                concat <$> zipWithM (\a b -> unifyOne generics $ ConsEq a b) ts1 ts2

        _ -> fail $ "cannot unify: " ++ show (t1, t2)

    ConsSpecial t1 t2 -> case (t1, t2) of
        _ -> return [] -- TODO, fill this in if needed


unify :: [Symbol] -> [Constraint] -> DoM ASTResolved [(Type, Type)]
unify generics []     = return []
unify generics (x:xs) = do
    subs <- unify generics xs
    s <- unifyOne generics (applyConstraint subs x)
    return (s ++ subs)


getConstraints :: CallHeader -> FuncBody -> DoM ASTResolved [Constraint]
getConstraints call body = do
    retCs <- getConstraintsFromTypes (funcGenerics body) (funcRetty body) (callRetType call)
    argCs <- fmap concat $ zipWithM (getConstraintsFromTypes $ funcGenerics body)
        (map typeof $ funcArgs body)
        (callArgTypes call)
    parCs <- case callParamType call of
        Nothing       -> return []
        Just (Type _) -> return []
        Just typ -> do
            let fts = map typeof (funcParams body)
            base <- baseTypeOf typ
            ts <- case base of
                Record ts -> return ts
                _         -> return [typ]
            unless (length fts == length ts) (error "param mismatch")
            fmap concat $ zipWithM (getConstraintsFromTypes (funcGenerics body)) fts ts
    return (retCs ++ argCs ++ parCs)
    


getConstraintsFromTypes :: [Symbol] -> Type -> Type -> DoM ASTResolved [Constraint]
getConstraintsFromTypes generics t1 t2 = do
    flatT1 <- flattenType t1
    flatT2 <- flattenType t2
    fromTypes flatT1 flatT2
    where
        fromTypes :: Type -> Type -> DoM ASTResolved [Constraint]
        fromTypes t1 t2 = do
            typedefs <- gets typeFuncs
            case (t1, t2) of
                (a, b) | a == b                               -> return [] 
                (Tuple (TypeApply s []), t) | elem s generics -> return [ConsSpecial t1 t2]
                (Tuple a, Tuple b)                            -> fromTypes a b
                (Table a, Table b)                            -> fromTypes a b
                (Type _, _)                                   -> return [ConsEq t1 t2]
                (_, Type _)                                   -> return []

                (RecordApply (TypeApply s ts), Record ts2)
                    | s `elem` generics -> return [ConsSpecial t1 t2]

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

                    | otherwise -> fail "here"

                (TypeApply s1 [], t) | elem s1 generics -> return [ConsEq t1 t2]

                _ -> error $ show (t1, t2)
