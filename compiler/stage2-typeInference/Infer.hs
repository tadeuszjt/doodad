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

-- Infer takes an ast and recursively runs the type inference algorithms until it can no longer
-- make any changes to the ast.


inferFunc :: FuncBody -> DoM ASTResolved FuncBody
inferFunc func = do
    annotatedFunc <- fmap fst $ withErrorPrefix "annotate: " $
        runDoMExcept 0 (annotateFunc func)
    collectState <- fmap snd $ withErrorPrefix "collect: " $
        runDoMExcept initCollectState (collectFuncDef annotatedFunc)
    subs <- unify $ Map.toList (collected collectState)
    let appliedFunc = applyFuncBody subs annotatedFunc
    fmap fst $ runDoMExcept () (deAnnotateFunc appliedFunc)


inferFuncDefaults :: FuncBody -> DoM ASTResolved FuncBody
inferFuncDefaults func = do
    annotatedFunc <- fmap fst $ withErrorPrefix "annotate: " $
        runDoMExcept 0 (annotateFunc func)
    collectState <- fmap snd $ withErrorPrefix "collect: " $
        runDoMExcept initCollectState (collectFuncDef annotatedFunc)
    subs <- unifyDefault $ Map.toList (defaults collectState)
    let appliedFunc = applyFuncBody subs annotatedFunc
    fmap fst $ runDoMExcept () (deAnnotateFunc appliedFunc)


infer :: ASTResolved -> Bool -> Bool -> DoM s (ASTResolved, Int)
infer ast printAnnotated verbose = runDoMUntilSameResult ast $ \ast -> do 
    inferred <- inferTypesPerFunc ast
    defaulted <- inferDefaults inferred
    fmap snd $ runDoMExcept defaulted (FunctionInstantiator.compile verbose)
    where
        inferTypesPerFunc :: ASTResolved -> DoM s ASTResolved
        inferTypesPerFunc ast = do
            funcDefs' <- forM (funcDefs ast) $ \func -> do
                case funcGenerics func of
                    [] -> fmap fst $ runDoMExcept ast $
                        fmap fst (runDoMUntilSameResult func inferFunc)
                    _  -> return func

            funcInstances' <- forM (funcInstances ast) $ \func -> do
                case funcGenerics func of
                    [] -> fmap fst $ runDoMExcept ast $
                        fmap fst (runDoMUntilSameResult func inferFunc)
                    _  -> return func

            return (ast { funcDefs = funcDefs', funcInstances = funcInstances' })


        inferDefaults :: ASTResolved -> DoM s ASTResolved
        inferDefaults ast = do
            funcDefs' <- forM (funcDefs ast) $ \func -> do
                case funcGenerics func of
                    [] -> fmap fst $ runDoMExcept ast $
                        fmap fst (runDoMUntilSameResult func inferFuncDefaults)
                    _  -> return func

            funcInstances' <- forM (funcInstances ast) $ \func -> do
                case funcGenerics func of
                    [] -> fmap fst $ runDoMExcept ast $
                        fmap fst (runDoMUntilSameResult func inferFuncDefaults)
                    _  -> return func

            return (ast { funcDefs = funcDefs', funcInstances = funcInstances' })
