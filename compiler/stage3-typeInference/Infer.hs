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
            funcDefs <- gets funcDefsAll
            modName <- gets moduleName

            forM_ (Map.toList $ funcDefs) $ \(symbol, func) ->
                when (symbolModule symbol == modName) $ do
                    let stmt = FuncDef [] func
                    (stmt', _) <- runDoMUntilSameResult stmt $ \stmt -> do
                        (stmtInferred, _) <- runDoMUntilSameResult stmt inferStmtTypes
                        fmap fst $ runDoMUntilSameResult stmtInferred inferStmtDefaults

                    let FuncDef _ func' = stmt'
                    modify $ \s -> s { funcDefsAll = Map.insert symbol func' (funcDefsAll s) }


            aquires <- gets aquiresAll
            forM_ (Map.toList aquires) $ \(symbol, stmt) ->
                when (symbolModule symbol == modName) $ do
                    (stmt', _) <- runDoMUntilSameResult stmt $ \stmt -> do
                        (stmtInferred, _) <- runDoMUntilSameResult stmt inferStmtTypes
                        fmap fst $ runDoMUntilSameResult stmtInferred inferStmtDefaults

                    modify $ \s -> s { aquiresAll = Map.insert symbol stmt' (aquiresAll s) }
