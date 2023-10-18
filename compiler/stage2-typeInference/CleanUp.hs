{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CleanUp where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST
import Symbol
import Monad
import Error
import Type
import ASTResolved
import Apply
import FunctionFinder
import ASTMapper


-- Resolves function calls
-- Creates generic instantiations
-- Resolves tuple/table field symbols
-- Turns ctor function call into Contructors
compile :: BoM ASTResolved m => m ()
compile = do
    funcDefs <- gets funcDefs
    forM_ (Map.toList funcDefs) $ \(symbol, body) -> do
        when (funcTypeArgs body == []) $ do
            stmt' <- (mapStmt cleanUpMapper) (funcStmt body)
            body' <- return body { funcStmt = stmt' }
            modify $ \s -> s { funcDefs = Map.insert symbol body' (ASTResolved.funcDefs s) }


genSymbol :: BoM ASTResolved m => String -> m Symbol
genSymbol sym = do  
    modName <- gets moduleName
    im <- gets $ Map.lookup sym . symSupply
    let n = maybe 0 (id) im
    modify $ \s -> s { symSupply = Map.insert sym (n + 1) (symSupply s) }
    let symbol = SymResolved modName sym n
    return symbol


cleanUpMapper :: BoM ASTResolved m => Elem -> m (Maybe Elem)
cleanUpMapper elem = case elem of
    ElemStmt (AST.FuncDef _ _ _ _ _ _ _) -> return Nothing
    ElemStmt (AST.Const _ _ _)           -> return Nothing
    ElemStmt (AST.Typedef _ _ _ _)       -> return Nothing

    ElemExpr (AExpr exprType expr@(AST.Field pos e symbol)) -> case symbol of
        Sym _             -> Just . ElemExpr . AExpr exprType <$> resolveFieldAccess expr
        SymResolved _ _ _ -> return $ Just $ ElemExpr $ AExpr exprType (Field pos e symbol)

    ElemExpr (AExpr exprType expr@(AST.Call pos params symbol exprs)) -> do
        symbol' <- resolveFuncCall exprType expr
        isCtor <- Map.member symbol' <$> gets ctorDefs
        if isCtor then do
            assert (params == []) "constructor cannot have params"
            return $ Just $ ElemExpr $ AExpr exprType (Construct pos symbol' exprs)
        else do
            return $ Just $ ElemExpr $ AExpr exprType (Call pos params symbol' exprs)

    ElemExpr    _ -> return (Just elem)
    ElemPattern _ -> return (Just elem)
    ElemType    _ -> return (Just elem)
    ElemStmt    _ -> return (Just elem)

    
-- add extern if needed
resolveFuncCall :: BoM ASTResolved m => Type -> AST.Expr -> m Symbol
resolveFuncCall exprType (AST.Call pos params symbol args) = withPos pos $ do
    let callHeader = FuncHeader [] (map typeof params) symbol (map typeof args) exprType
    ast <- get

    symbols <- case symbol of
        SymResolved _ _ _ -> return [symbol] -- already resolved
        _                 -> findCandidates callHeader ast

    exacts <- findExactFunction callHeader ast

    if exacts /= [] then do
        assert (length exacts == 1) "cannot have multiple exact matches"
        return $ head exacts
    else do -- TODO this part could be neater
        case symbols of
            [] -> do
                liftIO $ do
                    prettyASTResolved ast
                error $ "no candidates for:" ++ show callHeader
            [symbol'] -> do
                if (isGenericFunction symbol' ast) then do
                    let header  = getFunctionHeader symbol' ast
                    let body    = getFunctionBody symbol' ast
                    let sym     = Symbol.sym (ASTResolved.symbol header)
                    headerReplaced <- replaceGenericsInFuncHeaderWithCall header callHeader

                    if isJust headerReplaced && funcHeaderFullyResolved (fromJust headerReplaced) then do
                        exacts <- findExactFunction ((fromJust headerReplaced) { symbol = Symbol.Sym sym }) ast 
                        case exacts of 
                            [] -> do -- define specific type case
                                symbol'' <- genSymbol sym
                                body' <- replaceGenericsInFuncBodyWithCall body callHeader
                                modify $ \s -> s { funcDefs = Map.insert symbol'' body' (funcDefs ast) }
                                return symbol''
                            [symbol''] -> return symbol''
                    else return symbol'
                else do
                    return symbol'
            _ -> do

                return symbol


resolveFieldAccess :: BoM ASTResolved m => AST.Expr -> m Expr
resolveFieldAccess (AST.Field pos expr' (Sym sym)) = do
    -- (tup:typeSymbol).x:i64
    -- find mod_x_n
    ctors <- gets ctorDefs
    res <- fmap catMaybes $ forM (Map.toList ctors) $ \(symbol, (typeSymbol, i)) -> do
        exprTypeSymbolm <- case typeof expr' of
            Type.TypeApply s _ -> return (Just s)
            _ -> return Nothing
        case exprTypeSymbolm of
            Nothing -> return Nothing
            Just exprTypeSymbol -> do
                if Symbol.sym symbol == sym && exprTypeSymbol == typeSymbol then
                    return $ Just symbol
                else return Nothing

    case res of
        (a:b:xs) -> fail "ambiguous"
        []       -> return $ Field pos expr' (Sym sym)
        [symbol] -> return $ Field pos expr' symbol




