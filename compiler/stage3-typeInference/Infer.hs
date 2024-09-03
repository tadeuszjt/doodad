module Infer where

import qualified Data.Map as Map
import Control.Monad.State
import Monad
import Error
import Apply
import Unify
import Collect
import ASTResolved
import Annotate
import AST
import Symbol

-- Infer takes an ast and recursively runs the type inference algorithms until it can no longer
-- make any changes to the ast.

inferStmtTypes :: Stmt -> DoM ASTResolved Stmt
inferStmtTypes func = do
    --liftIO $ putStrLn "inferStmt"
    annotatedStmt <- fmap fst $ withErrorPrefix "annotate: " $
        runDoMExcept 0 (annotateStmt func)
    ast <- get
    collectState <- fmap snd $ withErrorPrefix "collect: " $
        runDoMExcept (initCollectState ast) (collectStmt annotatedStmt)
    subs <- unify $ Map.toList (collected collectState)
    let appliedStmt = applyStmt subs annotatedStmt
    fmap fst $ runDoMExcept () (deAnnotateStmt appliedStmt)


inferStmtDefaults :: Stmt -> DoM ASTResolved Stmt
inferStmtDefaults func = do
    --liftIO $ putStrLn "inferStmtDefaults"
    annotatedStmt <- fmap fst $ withErrorPrefix "annotate: " $
        runDoMExcept 0 (annotateStmt func)
    ast <- get
    collectState <- fmap snd $ withErrorPrefix "collect: " $
        runDoMExcept (initCollectState ast) (collectStmt annotatedStmt)
    subs <- unify $ Map.toList (defaults collectState)
    let appliedStmt = applyStmt subs annotatedStmt
    fmap fst $ runDoMExcept () (deAnnotateStmt appliedStmt)


infer :: ASTResolved -> Bool -> Bool -> DoM s ASTResolved
infer ast printAnnotated verbose = fmap snd $ runDoMExcept ast inferFuncs
    where
        inferFuncs :: DoM ASTResolved ()
        inferFuncs = do
            modName <- gets moduleName
            acquires <- gets acquiresAll
            forM_ (Map.toList acquires) $ \(featureSymbol, acqMap) -> do
                acqMap' <- fmap Map.fromList $ forM (Map.toList acqMap) $ \(symbol, stmt) ->
                    case symbolModule symbol == modName of
                        False -> return (symbol, stmt)
                        True  -> do
                            (stmt', _) <- runDoMUntilSameResult stmt $ \stmt -> do
                                (stmtInferred, _) <- runDoMUntilSameResult stmt inferStmtTypes
                                fmap fst $ runDoMUntilSameResult stmtInferred inferStmtDefaults
                            return (symbol, stmt')

                modify $ \s -> s { acquiresAll = Map.insert featureSymbol acqMap' (acquiresAll s) }
