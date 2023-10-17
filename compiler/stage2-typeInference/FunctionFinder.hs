module FunctionFinder where

import Data.Maybe
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

findCandidates :: (MonadIO m, MonadFail m) => FuncHeader -> ASTResolved -> m [Symbol]
findCandidates callHeader ast = do
    funcSymbols <- findFunctionCandidates callHeader ast
    typeSymbols <- findTypeCandidates callHeader ast
    ctorSymbols <- findCtorCandidates callHeader ast
    let candidates = Set.toList $ Set.fromList $ concat $ [funcSymbols, typeSymbols, ctorSymbols]
    return candidates


findFunctionCandidates :: MonadFail m => FuncHeader -> ASTResolved -> m [Symbol]
findFunctionCandidates callHeader ast = do
    fmap catMaybes $ forM (Map.toList $ Map.union (funcDefs ast) (funcImports ast)) $ \(symbol, body) -> do
        case funcHeadersCouldMatch (funcHeaderFromBody symbol body) callHeader of
            True -> return $ Just $ symbol
            False -> return Nothing

-- does not allow generic functions
findExactFunction :: Monad m => FuncHeader -> ASTResolved -> m [Symbol]
findExactFunction callHeader ast = do
    fmap catMaybes $ forM (Map.toList $ Map.union (funcDefs ast) (funcImports ast)) $ \(symbol, body) -> case funcTypeArgs body of
        [] -> case funcHeadersCouldMatch (funcHeaderFromBody symbol body) callHeader of
            True -> return $ Just $ symbol
            False -> return Nothing
        _ -> return Nothing


findTypeCandidates :: MonadFail m => FuncHeader -> ASTResolved -> m [Symbol]
findTypeCandidates callHeader ast = do
    let res     = Map.filterWithKey (\k v -> symbolsCouldMatch k $ symbol callHeader) (typeFuncs ast)
    return $ Map.keys res


findCtorCandidates :: MonadFail m => FuncHeader -> ASTResolved -> m [Symbol]
findCtorCandidates callHeader ast = do
    let res = Map.filterWithKey (\k v -> symbolsCouldMatch k $ symbol callHeader) (ctorDefs ast)
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
--            Table ts -> all (== True) (map typeFullyResolved ts)
            _ -> error $ "typeFullyResolved: " ++ show typ



-- using a modified version of unify to replace generic type variables in function keys
replaceGenericsInFuncHeaderWithCall :: BoM s m => FuncHeader -> FuncHeader -> m (Maybe FuncHeader) 
replaceGenericsInFuncHeaderWithCall header callHeader = do
    assert (typeArgs callHeader == []) "Call header cannot have type args"
    assert (funcHeadersCouldMatch header callHeader) "headers cannot match"
    resm <- catchError (fmap Just $ getConstraintsFromFuncHeaders header callHeader) (\_ -> return Nothing) 
    case resm of
        Nothing -> return Nothing
        Just constraints -> do
            subs <- unify (typeArgs header) constraints
            let header' = (applySubs subs header) { typeArgs = [] } 
            if funcHeaderHasGenerics (typeArgs header) header' then return Nothing
            else return $ Just header'


replaceGenericsInFuncBodyWithCall :: BoM s m => FuncBody -> FuncHeader -> m FuncBody
replaceGenericsInFuncBodyWithCall body callHeader = do
    let header = funcHeaderFromBody (symbol callHeader) body
    assert (typeArgs callHeader == []) "Call header cannot have type args"
    assert (funcHeadersCouldMatch callHeader header) "headers must be matchable"
    constraints <- getConstraintsFromFuncHeaders header callHeader
    subs <- unify (typeArgs header) constraints
    return $ (applySubs subs body) { funcTypeArgs = [] } 


unifyOne :: MonadFail m => [Symbol] -> Constraint -> m [(Type, Type)]
unifyOne typeVars constraint = case constraint of
    ConsEq t1 t2 -> case (t1, t2) of
        _ | t1 == t2                            -> return []
        --(TypeApply _ _, TypeApply _ _) -> error (show constraint)
        (TypeApply s [], _) | s `elem` typeVars -> return [(t1, t2)]
--        (_, TypeApply s []) | s `elem` typeVars -> return [(t2, t1)]
        (Type _, _)                             -> return [(t1, t2)]
        (_, Type _)                             -> return [(t2, t1)]
--        (Tuple ts1, Tuple ts2) | length ts1 == length ts2 -> fmap concat $
--            zipWithM (\x y -> unifyOne typeVars $ ConsEq x y) ts1 ts2
        _ -> error $ show (t1, t2)


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
--    (TypeApply s _, _) | s `elem` typeArgs -> error "don't know"
--    (TypeApply s _, Type _) -> return []
--
--    (TypeApply s1 t1, TypeApply s2 t2) -> do
--        assert (s1 == s2) "types should match"
--        getConstraintsFromTypes typeArgs t1 t2

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

    (Tuple _, I64) -> fail "invalid types"

--    (Table ts1, Table ts2) -> do
--        assert (length ts1 == length ts2) "types should be applied to same number of args"
--        fmap concat $ zipWithM (getConstraintsFromTypes typeArgs) ts1 ts2
--    (Void, _) -> return [(ConsEq typeToReplace typ)]

    _ -> error $ show (typeToReplace, typ)
