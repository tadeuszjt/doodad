module Infer where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Monad
import Error
import Apply
import Unify
import Collect
import ASTResolved
import Annotate
import FunctionInstantiator
import AST

-- Infer takes an ast and recursively runs the type inference algorithms until it can no longer
-- make any changes to the ast.


inferFuncTypes :: Func -> DoM ASTResolved Func
inferFuncTypes func = do
    --liftIO $ putStrLn "inferFunc"
    annotatedFunc <- fmap fst $ withErrorPrefix "annotate: " $
        runDoMExcept 0 (annotateFunc func)
    ast <- get
    collectState <- fmap snd $ withErrorPrefix "collect: " $
        runDoMExcept (initCollectState ast) (collectFuncDef annotatedFunc)
    subs <- unify $ Map.toList (collected collectState)
    let appliedFunc = applyFunc subs annotatedFunc
    fmap fst $ runDoMExcept () (deAnnotateFunc appliedFunc)


inferFuncDefaults :: Func -> DoM ASTResolved Func
inferFuncDefaults func = do
    --liftIO $ putStrLn "inferFuncDefaults"
    annotatedFunc <- fmap fst $ withErrorPrefix "annotate: " $
        runDoMExcept 0 (annotateFunc func)
    ast <- get
    collectState <- fmap snd $ withErrorPrefix "collect: " $
        runDoMExcept (initCollectState ast) (collectFuncDef annotatedFunc)
    subs <- unify $ Map.toList (defaults collectState)
    let appliedFunc = applyFunc subs annotatedFunc
    fmap fst $ runDoMExcept () (deAnnotateFunc appliedFunc)


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
            funcDefsTop <- gets funcDefsTop
            forM_ funcDefsTop $ \symbol -> do
                Just func <- gets (Map.lookup symbol . funcDefsAll)
                (func', _) <- runDoMUntilSameResult func $ \func -> do
                    (funcInferred, _) <- runDoMUntilSameResult func inferFuncTypes
                    fmap fst $ runDoMUntilSameResult funcInferred inferFuncDefaults

                modify $ \s -> s { funcDefsAll = Map.insert symbol func' (funcDefsAll s) }

            aquiresTop <- gets aquiresTop 
            forM_ aquiresTop $ \symbol -> do
                Just stmt <- gets (Map.lookup symbol . aquiresAll)
                (stmt', _) <- runDoMUntilSameResult stmt $ \stmt -> do
                    (stmtInferred, _) <- runDoMUntilSameResult stmt inferStmtTypes
                    fmap fst $ runDoMUntilSameResult stmtInferred inferStmtDefaults

                modify $ \s -> s { aquiresAll = Map.insert symbol stmt' (aquiresAll s) }
