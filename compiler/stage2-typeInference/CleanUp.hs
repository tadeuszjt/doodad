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
        Sym _             -> Just . ElemExpr . AExpr exprType . AST.Field pos e <$> resolveFieldAccess (typeof e) symbol
        SymResolved _ _ _ -> return $ Just $ ElemExpr $ AExpr exprType (Field pos e symbol)

    ElemExpr (AExpr exprType expr@(AST.Call pos params symbol exprs)) -> do
        symbol' <- resolveFuncCall exprType expr
        isCtor <- Map.member symbol' <$> gets ctorDefs
        fmap (Just . ElemExpr . AExpr exprType) $ case isCtor of
            False -> return (Call pos params symbol' exprs)
            True -> do
                assert (params == []) "constructor cannot have params"
                return (Construct pos symbol' exprs)

    ElemPattern (PatField pos symbol pats) -> do
        [symbol'] <- findCtorCandidates symbol
        return $ Just $ ElemPattern (PatField pos symbol' pats)

    _ -> return (Just elem)

    
-- add extern if needed
resolveFuncCall :: BoM ASTResolved m => Type -> AST.Expr -> m Symbol
resolveFuncCall exprType (AST.Call pos params callSymbol args) = withPos pos $ do
    let callHeader = FuncHeader [] (map typeof params) callSymbol (map typeof args) exprType
    candidates <- findCandidates callHeader
    ast <- get
    case candidates of
        --[] -> return callSymbol
        [] -> error $ "no candidates for: " ++ show callHeader

        [symbol] | isNonGenericFunction symbol ast -> return symbol
        [symbol] | isCtor symbol ast               -> return symbol
        [symbol] | isGenericFunction symbol ast    -> do -- this is where we replace
            let genericBody = getFunctionBody symbol ast
            resE <- tryError $ replaceGenericsInFuncBodyWithCall genericBody callHeader
            case resE of
                Left e -> do
                    liftIO $ putStrLn $ "warning: replaceGenericsInFuncBodyWithCall failed for: " ++ show callSymbol ++ " - " ++ show e
                    return callSymbol
                Right bodyReplaced -> case funcHeaderFullyResolved (funcTypeArgs genericBody) (funcHeaderFromBody symbol bodyReplaced) of
                    False -> return callSymbol
                    True  -> do
                        symbol' <- genSymbol (Symbol.sym callSymbol)
                        modify $ \s -> s { funcDefs = Map.insert symbol' bodyReplaced (funcDefs s) }
                        --liftIO $ putStrLn $ "replaced: " ++ show callSymbol ++ " with: " ++ show symbol'
                        --liftIO $ prettyFuncBody  symbol' bodyReplaced
                        return symbol'

        [genericSymbol, nonGenericSymbol] |
            isGenericFunction genericSymbol ast &&
            isNonGenericFunction nonGenericSymbol ast -> do
                let genericBody = getFunctionBody genericSymbol ast
                resE <- tryError $ replaceGenericsInFuncBodyWithCall genericBody callHeader
                case resE of
                    Left e -> do
                        liftIO $ putStrLn $ "warning: replaceGenericsInFuncBodyWithCall failed for: " ++ show callSymbol ++ " - " ++ show e
                        return callSymbol
                    Right bodyReplaced -> do
                        let genericHeader    = funcHeaderFromBody nonGenericSymbol bodyReplaced
                        let nonGenericHeader = funcHeaderFromBody nonGenericSymbol (getFunctionBody nonGenericSymbol ast)
                        if genericHeader == nonGenericHeader then
                            return nonGenericSymbol
                        else
                            return callSymbol


            

        _ -> do
            liftIO $ putStrLn $ "multiple candidates for: " ++ show candidates
            return callSymbol


-- (x:typ).sym -> (x:typ).mod_sym_0
resolveFieldAccess :: BoM ASTResolved m => Type -> Symbol -> m Symbol
resolveFieldAccess typ (Sym sym) = do
    typeDefs <- gets typeFuncs
    ctors    <- gets ctorDefs

    case typ of
        Type _ -> return (Sym sym)
        _ -> do
            let typeSymbol = getFieldAccessorSymbol typeDefs typ

            res <- fmap catMaybes $ forM (Map.toList ctors) $ \(symbol, (typSym, i)) -> do
                if Symbol.sym symbol == sym && typeSymbol == typSym then
                    return (Just symbol)
                else
                    return Nothing

            case res of
                (a:b:xs) -> fail "ambiguous"
                []       -> return $ (Sym sym)
                [symbol] -> return $ symbol




