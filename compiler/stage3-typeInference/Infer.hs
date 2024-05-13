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
import FunctionInstantiator
import AST

-- Infer takes an ast and recursively runs the type inference algorithms until it can no longer
-- make any changes to the ast.


inferFunc :: Func -> DoM ASTResolved Func
inferFunc func = do
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


infer :: ASTResolved -> Bool -> Bool -> DoM s (ASTResolved, Int)
infer ast printAnnotated verbose = runDoMUntilSameResult ast $ \ast -> do 
    inferred <- inferTypesPerFunc ast
    defaulted <- inferDefaults inferred
    FunctionInstantiator.compile verbose defaulted
    where
        inferTypesPerFunc :: ASTResolved -> DoM s ASTResolved
        inferTypesPerFunc ast = do
            funcInstance' <- forM (funcInstance ast) $ \func -> do
                fmap fst $ runDoMExcept ast $
                    fmap fst (runDoMUntilSameResult func inferFunc)

            return (ast { funcInstance = funcInstance' })


        inferDefaults :: ASTResolved -> DoM s ASTResolved
        inferDefaults ast = do
            funcInstance' <- forM (funcInstance ast) $ \func -> do
                fmap fst $ runDoMExcept ast $
                    fmap fst (runDoMUntilSameResult func inferFuncDefaults)

            return (ast { funcInstance = funcInstance' })
