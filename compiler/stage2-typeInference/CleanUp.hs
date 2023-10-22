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
import TupleDeleter


-- Resolves function calls
-- Creates generic instantiations
-- Resolves tuple/table field symbols
-- Turns ctor function call into Contructors
-- Removed spurious tuple types
compile :: BoM ASTResolved m => m ()
compile = do
    funcDefs <- gets funcDefs
    forM_ (Map.toList funcDefs) $ \(symbol, body) -> do
        when (funcTypeArgs body == []) $ do
            stmt' <- (mapStmt cleanUpMapper) (funcStmt body)
            body' <- return body { funcStmt = stmt' }
            modify $ \s -> s { funcDefs = Map.insert symbol body' (ASTResolved.funcDefs s) }

    deleteSingleTuples


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
        fmap (Just . ElemExpr . AExpr exprType) $ case isCtor of
            False -> return (Call pos params symbol' exprs)
            True -> do
                assert (params == []) "constructor cannot have params"
                return (Construct pos symbol' exprs)

    _ -> return (Just elem)

    
-- add extern if needed
resolveFuncCall :: BoM ASTResolved m => Type -> AST.Expr -> m Symbol
resolveFuncCall exprType (AST.Call pos params callSymbol args) = withPos pos $ do
    let callHeader = FuncHeader [] (map typeof params) callSymbol (map typeof args) exprType
    candidates <- findCandidates callHeader
    ast <- get
    case candidates of
        [] -> error "no candidates"
        [symbol] | isGenericFunction symbol ast -> do -- this is where we replace
            resE <- tryError $ replaceGenericsInFuncBodyWithCall (getFunctionBody symbol ast) callHeader
            case resE of
                Left _             -> do
                    liftIO $ putStrLn "warning: replaceGenericsInFuncBodyWithCall failed"
                    return symbol
                Right bodyReplaced -> case funcHeaderFullyResolved (funcHeaderFromBody symbol bodyReplaced) of
                    False -> return symbol
                    True  -> do
                        symbol' <- genSymbol (Symbol.sym callSymbol)
                        modify $ \s -> s { funcDefs = Map.insert symbol' bodyReplaced (funcDefs s) }
                        return symbol'

        [symbol] -> return symbol
        _        -> return callSymbol



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




