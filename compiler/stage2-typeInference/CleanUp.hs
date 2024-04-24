module CleanUp where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

import AST
import ASTResolved
import ASTMapper
import Symbol
import Monad
import Error
import Type
import Apply
import FunctionFinder


-- Resolves function calls
-- Creates generic instantiations
compile :: Bool -> DoM ASTResolved ()
compile verbose = do
    --when verbose $ liftIO $ putStrLn $ "cleaning..."
    funcInstances <- gets funcInstances
    forM_ (Map.toList funcInstances) $ \(symbol, body) -> do
        when (funcGenerics body == []) $ do
            stmt' <- (mapStmtM cleanUpMapper) (funcStmt body)
            body' <- return body { funcStmt = stmt' }
            modify $ \s -> s { funcInstances = Map.insert symbol body' (ASTResolved.funcInstances s) }

    funcDefs <- gets funcDefs
    forM_ (Map.toList funcDefs) $ \(symbol, body) -> do
        when (funcGenerics body == []) $ do
            stmt' <- (mapStmtM cleanUpMapper) (funcStmt body)
            body' <- return body { funcStmt = stmt' }
            modify $ \s -> s { funcDefs = Map.insert symbol body' (ASTResolved.funcDefs s) }


genSymbol :: String -> DoM ASTResolved Symbol
genSymbol sym = do  
    modName <- gets moduleName
    im <- gets $ Map.lookup sym . symSupply
    let n = maybe 0 (id) im
    modify $ \s -> s { symSupply = Map.insert sym (n + 1) (symSupply s) }
    return (SymResolved modName sym n)


cleanUpMapper :: Elem -> DoM ASTResolved Elem
cleanUpMapper elem = case elem of
    ElemExpr (AExpr exprType expr@(AST.Call pos symbol exprs)) | all isAnnotated exprs -> do
        symbol' <- resolveFuncCall exprType expr
        fmap (ElemExpr . AExpr exprType) $ return (Call pos symbol' exprs)

    _ -> return elem
    where
        isAnnotated :: AST.Expr -> Bool
        isAnnotated (AExpr _ _) = True
        isAnnotated _           = False

    
-- add extern if needed
resolveFuncCall :: Type -> AST.Expr -> DoM ASTResolved Symbol
resolveFuncCall _ (AST.Call _ s@(SymResolved _ _ _) _) = return s
resolveFuncCall exprType (AST.Call pos calledSymbol args) = withPos pos $ do
    --liftIO $ putStrLn $ "resolving: " ++ show calledSymbol
    let callHeader = CallHeader calledSymbol (map typeof args) exprType

    candidates <- map callSymbol <$> findCandidates callHeader
    ast <- get

    case candidates of
        [] -> fail $ "no candidates for: " ++ show callHeader

        [symbol] | isNonGenericFunction symbol ast -> return symbol
        [symbol] | isGenericFunction symbol ast    -> do -- this is where we replace
            bodyReplaced <- replaceGenericsInFuncBodyWithCall
                (getFunctionBody symbol ast)
                callHeader

            case funcFullyResolved bodyReplaced of
                False -> return calledSymbol
                True  -> do
                    instancem <- findInstance $ CallHeader
                        symbol
                        (map typeof $ funcArgs bodyReplaced)
                        (typeof $ funcRetty bodyReplaced)
                    case instancem of
                        Just s -> return s
                        Nothing -> do
                            symbol' <- genSymbol (Symbol.sym calledSymbol)
                            modify $ \s -> s { funcInstances = Map.insert symbol' bodyReplaced (funcInstances s) }
                            return symbol'

        _ -> return (calledSymbol)
