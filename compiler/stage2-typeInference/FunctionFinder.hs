module FunctionFinder where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad

import Control.Monad.Fail
import ASTResolved
import Symbol
import Type
import Apply

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
findFunctionCandidates callKey@(paramTypes, symbol, argTypes, returnType) ast = do
    funcDefsGenericReplaced <- fmap Map.fromList $ forM (Map.toList $ funcDefsGeneric ast) $ \(symbol, body) -> do
        key' <- replaceGenericsInFuncKey (funcTypeArgs body) (funcKeyFromBody symbol body)
        return (symbol, key')

    let resGeneric = Map.filterWithKey (\k v -> funcKeysCouldMatch v callKey) funcDefsGenericReplaced
    let allMaps = Map.unions [funcImports ast, funcDefs ast]
    let res     = Map.filterWithKey (\k v -> funcKeysCouldMatch (funcKeyFromBody k v) callKey) allMaps
    let res'    = Map.mapWithKey (\k v -> funcKeyFromBody k v) res

    return $ Map.keys $ Map.union resGeneric res'


findTypeCandidates :: MonadFail m => FuncKey -> ASTResolved -> m [Symbol]
findTypeCandidates callKey@(paramTypes, symbol, argTypes, returnType) ast = do
    let res     = Map.filterWithKey (\k v -> symbolsCouldMatch k symbol) (typeFuncs ast)
    return $ Map.keys res


findCtorCandidates :: MonadFail m => FuncKey -> ASTResolved -> m [Symbol]
findCtorCandidates callKey@(paramTypes, symbol, argTypes, returnType) ast = do
    let res     = Map.filterWithKey (\k v -> symbolsCouldMatch k symbol) (ctorDefs ast)
    return $ Map.keys res


-- turns a generic func key into one with the generics replaced with type variables.
replaceGenericsInFuncKey :: Monad m => [Symbol] -> FuncKey -> m FuncKey
replaceGenericsInFuncKey typeArgs (paramTypes, symbol, argTypes, retType) = do
    ts <- replaceGenerics typeArgs $ paramTypes ++ argTypes ++ [retType]
    let (ps, ts')  = (take (length paramTypes) ts, drop (length paramTypes) ts)
    let (as, ts'') = (take (length argTypes) ts', drop (length argTypes) ts') 
    let [rt]       = take 1 ts''
    return $ (ps, symbol, as, rt)


-- replaces generics with type variables
replaceGenerics :: Monad m => [Symbol] -> [Type] -> m [Type]
replaceGenerics typeArgs ts = do
    setOfGenerics <- return $ Set.fromList $ concat $ map (findGenerics typeArgs) ts
    substitutions <- forM (zip (Set.toList setOfGenerics) [-1, -2..]) $ \(g, x) -> do
        return (g, Type x)
    return $ map (applySubs substitutions) ts
    where
        findGenerics :: [Symbol] -> Type -> [Type]
        findGenerics typeArgs typ = case typ of
            TypeApply s [] -> if s `elem` typeArgs then [typ] else []
            TypeApply s _  -> if s `elem` typeArgs then error "Can't do this" else []
            _ -> error $ show typ
