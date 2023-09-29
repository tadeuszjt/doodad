module FunctionFinder where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad

import Control.Monad.Fail
import ASTResolved
import Symbol
import Type
import Apply
import Error
import Constraint
import Monad

--        , ctorDefs    :: Map.Map Symbol (Symbol, Int)    -- defined ctors
--        , funcImports :: Map.Map Symbol FuncBody          -- imported funcs
--        , funcDefs    :: Map.Map Symbol FuncBody         -- defined functions
--        , funcDefsGeneric :: Map.Map Symbol FuncBody 

--        , typeFuncs   :: Map.Map Symbol ([Symbol], Type) -- defined type functions
--        , ctorDefs    :: Map.Map Symbol (Symbol, Int)    -- defined ctors
findCandidates :: MonadFail m => FuncKey -> ASTResolved -> m [Symbol]
findCandidates callKey@(paramTypes, symbol, argTypes, returnType) ast = do
    funcSymbols <- findFunctionCandidates callKey ast
    typeSymbols <- findTypeCandidates callKey ast
    ctorSymbols <- findCtorCandidates callKey ast
    return $ Set.toList $ Set.fromList $ concat $ [funcSymbols, typeSymbols, ctorSymbols]


findFunctionCandidates :: MonadFail m => FuncKey -> ASTResolved -> m [Symbol]
findFunctionCandidates callKey ast = do
    fmap catMaybes $ forM (Map.toList $ Map.union (funcDefs ast) (funcImports ast)) $ \(symbol, body) -> do
        case funcKeysCouldMatch (funcTypeArgs body) (funcKeyFromBody symbol body) callKey of
            True -> return $ Just $ symbol
            False -> return Nothing


findTypeCandidates :: MonadFail m => FuncKey -> ASTResolved -> m [Symbol]
findTypeCandidates callKey@(paramTypes, symbol, argTypes, returnType) ast = do
    let res     = Map.filterWithKey (\k v -> symbolsCouldMatch k symbol) (typeFuncs ast)
    return $ Map.keys res


findCtorCandidates :: MonadFail m => FuncKey -> ASTResolved -> m [Symbol]
findCtorCandidates callKey@(paramTypes, symbol, argTypes, returnType) ast = do
    let res     = Map.filterWithKey (\k v -> symbolsCouldMatch k symbol) (ctorDefs ast)
    return $ Map.keys res


replaceGenericsInFuncKeyWithCall :: BoM s m => [Symbol] -> FuncKey -> FuncKey -> m FuncKey
replaceGenericsInFuncKeyWithCall typeArgs funcKey callKey = do
    assert (funcKeysCouldMatch typeArgs funcKey callKey) "funcKeys cannot match"
    constraints <- getConstraintsFromFuncKeys typeArgs funcKey callKey
    subs <- unify typeArgs constraints
    return $ applySubs subs funcKey


-- using a modified version of unify to replace generic type variables in function keys
unifyOne :: MonadFail m => [Symbol] -> Constraint -> m [(Type, Type)]
unifyOne typeVars constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                            -> return []
        (TypeApply s [], _) | s `elem` typeVars -> return [(t1, t2)]
        (_, TypeApply s []) | s `elem` typeVars -> return [(t2, t1)]
        (Type _, _)                             -> return [(t1, t2)]
        (_, Type _)                             -> return [(t2, t1)]


unify :: MonadFail m => [Symbol] -> [Constraint] -> m [(Type, Type)]
unify typeVars []     = return []
unify typeVars (x:xs) = do
    subs <- unify typeVars xs
    s <- unifyOne typeVars (applySubs subs x)
    return (s ++ subs)


getConstraintsFromFuncKeys :: MonadFail m => [Symbol] -> FuncKey -> FuncKey -> m [Constraint]
getConstraintsFromFuncKeys typeArgs keyToReplace key = do
    let (paramTypes1, symbol1, argTypes1, retty1) = keyToReplace
    let (paramTypes2, symbol2, argTypes2, retty2) = key
    paramTypesConstraints <- fmap concat $ zipWithM (getConstraintsFromTypes typeArgs) paramTypes1 paramTypes2
    argTypesConstraints <- fmap concat $ zipWithM (getConstraintsFromTypes typeArgs) argTypes1 argTypes2
    rettyConstraints <- getConstraintsFromTypes typeArgs retty1 retty2
    return $ paramTypesConstraints ++ argTypesConstraints ++ rettyConstraints


getConstraintsFromTypes :: MonadFail m => [Symbol] -> Type -> Type -> m [Constraint]
getConstraintsFromTypes typeArgs typeToReplace typ = case (typeToReplace, typ) of
    (TypeApply s [], _) | s `elem` typeArgs -> return [ConsEq typeToReplace typ]
    _ -> error $ show (typeToReplace, typ)
