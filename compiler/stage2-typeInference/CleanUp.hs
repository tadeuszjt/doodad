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
    ElemExpr (AExpr exprType expr@(AST.Call pos symbol exprs)) -> do
        symbol' <- resolveFuncCall exprType expr
        fmap (ElemExpr . AExpr exprType) $ return (Call pos symbol' exprs)

    _ -> return elem

    
-- add extern if needed
resolveFuncCall :: Type -> AST.Expr -> DoM ASTResolved Symbol
resolveFuncCall _ (AST.Call _ s@(SymResolved _ _ _) _) = return s
resolveFuncCall exprType (AST.Call pos callSymbol args) = withPos pos $ do
    --liftIO $ putStrLn $ "resolving: " ++ show callSymbol
    let callHeader = CallHeader callSymbol (map typeof args) exprType

    candidates <- findCandidates callHeader
    ast <- get
    case candidates of
        [] -> fail $ "no candidates for: " ++ show callHeader

        [symbol] | isNonGenericFunction symbol ast -> return symbol
        [symbol] | isGenericFunction symbol ast    -> do -- this is where we replace
            let genericBody = getFunctionBody symbol ast
            bodyReplaced <- replaceGenericsInFuncBodyWithCall genericBody callHeader
            --liftIO $ putStrLn $ "new body: " ++ show bodyReplaced
            case funcFullyResolved (funcGenerics genericBody) bodyReplaced of
                False -> return callSymbol
                True  -> do
                    symbol' <- genSymbol (Symbol.sym callSymbol)
                    modify $ \s -> s { funcDefs = Map.insert symbol' bodyReplaced (funcDefs s) }
                    return symbol'

        [genericSymbol, nonGenericSymbol] |
            isGenericFunction genericSymbol ast &&
            isNonGenericFunction nonGenericSymbol ast -> do
                let genericBody = getFunctionBody genericSymbol ast
                let nonGenericBody = getFunctionBody nonGenericSymbol ast
                bodyReplaced <- replaceGenericsInFuncBodyWithCall genericBody callHeader
                case funcHeaderTypesMatch bodyReplaced nonGenericBody of
                    True -> return nonGenericSymbol
                    False -> return callSymbol

        [nonGenericSymbol, genericSymbol] |
            isGenericFunction genericSymbol ast &&
            isNonGenericFunction nonGenericSymbol ast -> do
                let nonGenericBody = getFunctionBody nonGenericSymbol ast
                let genericBody = getFunctionBody genericSymbol ast
                bodyReplaced <- replaceGenericsInFuncBodyWithCall genericBody callHeader
                case funcHeaderTypesMatch bodyReplaced nonGenericBody of
                    True -> return nonGenericSymbol
                    False -> return callSymbol

        _ -> return callSymbol
