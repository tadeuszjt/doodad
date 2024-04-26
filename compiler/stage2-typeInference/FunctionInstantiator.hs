module FunctionInstantiator where

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
            stmt' <- (mapStmtM instantiatorMapper) (funcStmt body)
            body' <- return body { funcStmt = stmt' }
            modify $ \s -> s { funcInstances = Map.insert symbol body' (ASTResolved.funcInstances s) }

    funcDefs <- gets funcDefs
    forM_ (Map.toList funcDefs) $ \(symbol, body) -> do
        when (funcGenerics body == []) $ do
            stmt' <- (mapStmtM instantiatorMapper) (funcStmt body)
            body' <- return body { funcStmt = stmt' }
            modify $ \s -> s { funcDefs = Map.insert symbol body' (ASTResolved.funcDefs s) }


genSymbol :: String -> DoM ASTResolved Symbol
genSymbol sym = do  
    modName <- gets moduleName
    im <- gets $ Map.lookup sym . symSupply
    let n = maybe 0 (id) im
    modify $ \s -> s { symSupply = Map.insert sym (n + 1) (symSupply s) }
    return (SymResolved modName sym n)


instantiatorMapper :: Elem -> DoM ASTResolved Elem
instantiatorMapper elem = case elem of
    ElemExpr (AExpr exprType expr@(AST.Call pos symbol exprs)) | all isAnnotated exprs -> do
        symbol' <- withPos pos $ resolveFuncCall symbol (map typeof exprs) exprType
        fmap (ElemExpr . AExpr exprType) $ return (Call pos symbol' exprs)

    ElemPattern (PatAnnotated (PatIdent pos symbol) patType) -> do
        void $ resolveFuncCall (Sym "set") [patType, patType] Void
        return elem

    ElemPattern (PatAnnotated (PatTuple pos pats) patType) | all patAnnotated pats -> do
        when (length pats > 0) $ void $ resolveFuncCall (Sym "first") [patType] (typeof $ pats !! 0)
        when (length pats > 1) $ void $ resolveFuncCall (Sym "second") [patType] (typeof $ pats !! 1)
        when (length pats > 2) $ void $ resolveFuncCall (Sym "third") [patType] (typeof $ pats !! 2)
        when (length pats > 3) $ void $ resolveFuncCall (Sym "fourth") [patType] (typeof $ pats !! 3)
        return elem

    ElemPattern (PatAnnotated (PatSlice pos pats) patType) -> do
        when (length pats > 0) $ do
            void $ resolveFuncCall (Sym "at") [patType, I64] (typeof $ head pats)
        void $ resolveFuncCall (Sym "len") [patType] I64
        return elem

    ElemPatternIsolated (PatAnnotated (PatIdent pos symbol) patType) -> do
        return elem

    ElemPatternIsolated (PatAnnotated (PatSlice pos pats) patType) -> do
        fail "what"
        return elem

    _ -> return elem
    where
        patAnnotated :: AST.Pattern -> Bool
        patAnnotated (PatAnnotated _ t) = True
        patAnnotated _                  = False


        isAnnotated :: AST.Expr -> Bool
        isAnnotated (AExpr _ _) = True
        isAnnotated _           = False


resolveFuncCall :: Symbol -> [Type] -> Type -> DoM ASTResolved Symbol
resolveFuncCall s@(SymResolved _ _ _) argTypes retType = return s
resolveFuncCall calledSymbol        argTypes retType = do

    let callHeader = CallHeader calledSymbol argTypes retType

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
                    instancem <- findInstance ast $ CallHeader
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
