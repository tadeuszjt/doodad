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

findCandidates :: MonadFail m => FuncHeader -> ASTResolved -> m [Symbol]
findCandidates callHeader ast = do
    funcSymbols <- findFunctionCandidates callHeader ast
    typeSymbols <- findTypeCandidates callHeader ast
    ctorSymbols <- findCtorCandidates callHeader ast
    return $ Set.toList $ Set.fromList $ concat $ [funcSymbols, typeSymbols, ctorSymbols]


findFunctionCandidates :: MonadFail m => FuncHeader -> ASTResolved -> m [Symbol]
findFunctionCandidates callHeader ast = do
    fmap catMaybes $ forM (Map.toList $ Map.union (funcDefs ast) (funcImports ast)) $ \(symbol, body) -> do
        case funcHeadersCouldMatch (funcHeaderFromBody symbol body) callHeader of
            True -> return $ Just $ symbol
            False -> return Nothing


findTypeCandidates :: MonadFail m => FuncHeader -> ASTResolved -> m [Symbol]
findTypeCandidates callHeader ast = do
    let res     = Map.filterWithKey (\k v -> symbolsCouldMatch k $ symbol callHeader) (typeFuncs ast)
    return $ Map.keys res


findCtorCandidates :: MonadFail m => FuncHeader -> ASTResolved -> m [Symbol]
findCtorCandidates callHeader ast = do
    let res = Map.filterWithKey (\k v -> symbolsCouldMatch k $ symbol callHeader) (ctorDefs ast)
    return $ Map.keys res


-- using a modified version of unify to replace generic type variables in function keys
replaceGenericsInFuncHeaderWithCall :: BoM s m => FuncHeader -> FuncHeader -> m FuncHeader
replaceGenericsInFuncHeaderWithCall header callHeader = do
    assert (typeArgs callHeader == []) "Call header cannot have type args"
    assert (funcHeadersCouldMatch header callHeader) "headers cannot match"
    constraints <- getConstraintsFromFuncHeaders header callHeader
    subs <- unify (typeArgs header) constraints
    return $ (applySubs subs header) { typeArgs = [] } 


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


getConstraintsFromFuncHeaders :: MonadFail m => FuncHeader -> FuncHeader -> m [Constraint]
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


getConstraintsFromTypes :: MonadFail m => [Symbol] -> Type -> Type -> m [Constraint]
getConstraintsFromTypes typeArgs typeToReplace typ = case (typeToReplace, typ) of
    (TypeApply s [], _) | s `elem` typeArgs -> return [ConsEq typeToReplace typ]
    _ -> error $ show (typeToReplace, typ)
