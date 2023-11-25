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
    funcSymbols <- findFunctionCandidates call
    typeSymbols <- findTypeCandidates     call
    ctorSymbols <- findCtorCandidates (callSymbol call)
    let r = Set.toList $ Set.fromList $ concat $ [funcSymbols, typeSymbols, ctorSymbols]
    --liftIO $ putStrLn $ show call ++ ": " ++ show r
    return r



findFunctionCandidates :: CallHeader -> DoM ASTResolved [Symbol]
findFunctionCandidates call = do
    ast <- get
    fmap catMaybes $ forM (Map.toList $ Map.union (funcDefs ast) (funcImports ast)) $
        \(symbol, body) -> do
            b <- callCouldMatchFunc call symbol body
            return $ case b of
                True -> Just symbol
                False -> Nothing


findTypeCandidates :: CallHeader -> DoM ASTResolved [Symbol]
findTypeCandidates (CallHeader mReceiverType s argTypes returnType) = do
    typeFuncs <- gets typeFuncs
    let res     = Map.filterWithKey (\k v -> symbolsCouldMatch k s) typeFuncs
    return $ Map.keys res


findCtorCandidates :: Symbol -> DoM ASTResolved [Symbol]
findCtorCandidates callSymbol = do
    ctorDefs <- gets ctorDefs
    return $ Map.keys $ Map.filterWithKey (\k v -> symbolsCouldMatch k callSymbol) ctorDefs


callCouldMatchFunc :: CallHeader -> Symbol -> FuncBody -> DoM ASTResolved Bool
callCouldMatchFunc call symbol body
    | not $ symbolsCouldMatch (callSymbol call) symbol = return False
    | not $ length (callArgTypes call) == length (map typeof $ funcArgs body) = return False
    | otherwise = do
        ast <- get
        let argsMatch        = all (== True) $ zipWith (typesCouldMatch (typeFuncs ast) (funcGenerics body)) (callArgTypes call) (map typeof $ funcArgs body)
        let rettyMatch       = typesCouldMatch (typeFuncs ast) (funcGenerics body) (callRetType call) (funcRetty body)
        paramMatches <- case (callParamType call, map typeof (funcParams body)) of
            (Nothing, [])       -> return True
            (Nothing, _ )       -> return False
            (Just (Type _), []) -> return False
            (Just (Type _), _ ) -> return True
            (Just t1,  [t2])    -> return $ typesCouldMatch (typeFuncs ast) (funcGenerics body) t1 t2
            (Just t1,  t2s)     -> do
                baseT1 <- baseTypeOf t1
                t1s <- case baseT1 of
                    Record _ -> getRecordTypes t1
                    t        -> return [t]
                return $ all (== True) $ zipWith (typesCouldMatch (typeFuncs ast) (funcGenerics body)) t1s t2s
            x -> error (show x)
        return (argsMatch && rettyMatch && paramMatches)


funcFullyResolved :: [Symbol] -> FuncBody -> Bool
funcFullyResolved generics body =
    all (== True) (map typeFullyResolved $ map typeof $ funcParams body)
    && all (== True) (map typeFullyResolved $ map typeof $ funcArgs body)
    && typeFullyResolved (funcRetty body)
    where
        typeFullyResolved :: Type -> Bool
        typeFullyResolved typ = case typ of
            TypeApply s _ | elem s generics -> False
            TypeApply s ts -> all (== True) (map typeFullyResolved ts)
            _ | isSimple typ -> True
            Type _ -> False
            Void -> True
            Table t -> typeFullyResolved t
            Tuple t -> typeFullyResolved t
            Record ts -> all (== True) (map typeFullyResolved ts)
            RecordApply t -> typeFullyResolved t
            _ -> error $ "typeFullyResolved: " ++ show typ



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
        (TypeApply s _, _) | s `elem` generics  -> return [(t1, t2)]
        (Type _, _)                             -> return [(t1, t2)]
        (_, Type _)                             -> return [(t2, t1)]
        (Tuple a, Tuple b)                      -> unifyOne generics $ ConsEq a b

        (TypeApply s1 ts1, TypeApply s2 ts2)
            | length ts1 == length ts2 ->
                concat <$> zipWithM (\a b -> unifyOne generics $ ConsEq a b) ts1 ts2

        _ -> fail $ "cannot unify: " ++ show (t1, t2)

    ConsSpecial t1 t2 -> case (t1, t2) of
        _ -> return [] -- TODO, fill this in if needed
        _ -> error $ show (t1, t2)


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
        Nothing -> return []
        Just typ -> do
            case map typeof (funcParams body) of
                [t] -> getConstraintsFromTypes (funcGenerics body) t typ
                _ -> error "TODO"
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
                (a, b) | a == b    -> return [] 
                (Table a, Table b) -> fromTypes a b

                (Tuple (TypeApply s []), t) | elem s generics -> return [ConsSpecial t1 t2]
                (Tuple a, Tuple b) -> fromTypes a b

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
                (Type _, _)                             -> return [ConsEq t1 t2]
                (_, Type _)                             -> return []

                _ -> error $ show (t1, t2)
