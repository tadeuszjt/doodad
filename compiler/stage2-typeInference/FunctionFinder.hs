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
import ASTMapper
import TupleDeleter

findCandidates :: BoM ASTResolved m => FuncHeader -> m [Symbol]
findCandidates callHeader = do
    funcSymbols <- findFunctionCandidates callHeader
    typeSymbols <- findTypeCandidates callHeader
    ctorSymbols <- findCtorCandidates callHeader
    return $ Set.toList $ Set.fromList $ concat $ [funcSymbols, typeSymbols, ctorSymbols]


findFunctionCandidates :: BoM ASTResolved m => FuncHeader -> m [Symbol]
findFunctionCandidates callHeader = do
    ast <- get
    fmap catMaybes $ forM (Map.toList $ Map.union (funcDefs ast) (funcImports ast)) $ \(symbol, body) -> do
        case funcHeadersCouldMatch ast (funcHeaderFromBody symbol body) callHeader of
            True -> return $ Just $ symbol
            False -> return Nothing

findTypeCandidates :: BoM ASTResolved m => FuncHeader -> m [Symbol]
findTypeCandidates callHeader = do
    typeFuncs <- gets typeFuncs
    let res     = Map.filterWithKey (\k v -> symbolsCouldMatch k $ symbol callHeader) typeFuncs
    return $ Map.keys res


findCtorCandidates :: BoM ASTResolved m => FuncHeader -> m [Symbol]
findCtorCandidates callHeader = do
    ctorDefs <- gets ctorDefs
    let res = Map.filterWithKey (\k v -> symbolsCouldMatch k $ symbol callHeader) ctorDefs
    return $ Map.keys res


funcHeaderHasGenerics :: [Symbol] -> FuncHeader -> Bool
funcHeaderHasGenerics typeArgs header =
    paramGenerics /= [] || argGenerics /= [] || rettyGenerics /= []
    where
        paramGenerics = concat $ map (findGenerics typeArgs) (paramTypes header)
        argGenerics   = concat $ map (findGenerics typeArgs) (argTypes header)
        rettyGenerics = findGenerics typeArgs (returnType header)


funcHeaderFullyResolved :: FuncHeader -> Bool
funcHeaderFullyResolved header =
    typeArgs header == []
    && all (== True) (map typeFullyResolved $ paramTypes header)
    && all (== True) (map typeFullyResolved $ argTypes header)
    && typeFullyResolved (returnType header)
    where
        typeFullyResolved :: Type -> Bool
        typeFullyResolved typ = case typ of
            TypeApply s _ | s `elem` (typeArgs header) -> False
            TypeApply s ts -> all (== True) (map typeFullyResolved ts)
            _ | isSimple typ -> True
            Type _ -> False
            Void -> True
            Table t -> typeFullyResolved t
            Tuple t -> typeFullyResolved t
            Record ts -> all (== True) (map typeFullyResolved ts)
            _ -> error $ "typeFullyResolved: " ++ show typ


replaceGenericsInFuncBodyWithCall :: BoM ASTResolved m => FuncBody -> FuncHeader -> m FuncBody
replaceGenericsInFuncBodyWithCall body callHeader = do
    ast <- get
    let header = funcHeaderFromBody (symbol callHeader) body
    assert (typeArgs callHeader == []) "Call header cannot have type args"
    assert (funcHeadersCouldMatch ast callHeader header) "headers must be matchable"
    constraints <- getConstraintsFromFuncHeaders header callHeader
    subs <- unify (typeArgs header) constraints
    body' <- applySubs subs body
    mapFuncBody tupleDeleterMapper $ body' { funcTypeArgs = [] }


unifyOne :: BoM s m => [Symbol] -> Constraint -> m [(Type, Type)]
unifyOne typeVars constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                            -> return []
        (TypeApply s [], _) | s `elem` typeVars -> return [(t1, t2)]
        (Type _, _)                             -> return [(t1, t2)]
        (_, Type _)                             -> return [(t2, t1)]

        (Record _, Tuple _) -> fail "cannot unify"
        _ -> error $ show (t1, t2)


unify :: BoM s m => [Symbol] -> [Constraint] -> m [(Type, Type)]
unify typeVars []     = return []
unify typeVars (x:xs) = do
    subs <- unify typeVars xs
    s <- unifyOne typeVars =<< applySubs subs x
    return (s ++ subs)


getConstraintsFromFuncHeaders :: BoM ASTResolved m => FuncHeader -> FuncHeader -> m [Constraint]
getConstraintsFromFuncHeaders headerToReplace header = do
    assert (typeArgs header == []) "only headerToReplace can have typeArgs"
    paramConstraints <- fmap concat $
        zipWithM (getConstraintsFromTypes $ typeArgs headerToReplace)
            (paramTypes headerToReplace)
            (paramTypes header)
    argConstraints <- fmap concat $
        zipWithM (getConstraintsFromTypes $ typeArgs headerToReplace)
            (argTypes headerToReplace)
            (argTypes header)
    rettyConstraints <- getConstraintsFromTypes (typeArgs headerToReplace)
        (returnType headerToReplace)
        (returnType header)
    return $ paramConstraints ++ argConstraints ++ rettyConstraints


getConstraintsFromTypes :: BoM ASTResolved m => [Symbol] -> Type -> Type -> m [Constraint]
getConstraintsFromTypes typeArgs typeToReplace typ = do
    typedefs <- gets typeFuncs
    case (flattenTuple typedefs typeToReplace, flattenTuple typedefs typ) of
        (TypeApply s1 ts1, TypeApply s2 ts2)
            | s1 `elem` typeArgs -> do 
                assert (not $ s2 `elem` typeArgs) "don't know"
                assert (length ts1 == length ts2) "type argument lengths mismatch"
                constraints <- fmap concat $ zipWithM (getConstraintsFromTypes typeArgs) ts1 ts2
                return $ ConsEq typeToReplace typ : constraints

            | not (s1 `elem` typeArgs) -> do
                assert (not $ s2 `elem` typeArgs) "don't know"
                assert (length ts1 == length ts2) "type argument lengths mismatch"
                fmap concat $ zipWithM (getConstraintsFromTypes typeArgs) ts1 ts2

        (TypeApply s1 [], _)
            | s1 `elem` typeArgs -> return [ConsEq typeToReplace typ]

        (t, Type x) -> return [(ConsEq (Type x) t)]
        (Type x, t) -> return [(ConsEq (Type x) t)]

        (Table t1, Table t2) -> getConstraintsFromTypes typeArgs t1 t2
        (Tuple t1, Tuple t2) -> getConstraintsFromTypes typeArgs t1 t2

        (Void, Void) -> return []

        (a, b) | isSimple a && isSimple b -> return []

        (Tuple _, t) | isSimple t -> fail "invalid types"

        _ -> error $ show (typeToReplace, typ)
