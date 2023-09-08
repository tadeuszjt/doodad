module Infer where

import System.FilePath
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import qualified SymTab
import AST
import Args
import Monad
import Error
import Apply
import Unify
import Collect
import qualified Resolve
import ASTResolved
import Annotate
import DeAnnotate
import CleanUp
import Type

-- Takes a resolved and annotated ast and inferes all types.
infer :: BoM s m => ASTResolved -> Bool -> Bool -> m (ASTResolved, Int)
infer ast printAnnotated verbose = do 
    runBoMUntilSameResult ast $ \ast' -> do 
        (inferred, _) <- runBoMUntilSameResult ast' inferTypes
        (defaulted, _) <- runBoMUntilSameResult inferred inferDefaults
        return defaulted
    where
        inferTypes :: BoM s m => ASTResolved -> m ASTResolved
        inferTypes ast = do
            (annotated, typeSupplyCount) <- withErrorPrefix "annotate: " $
                runBoMTExcept 0 $ annotate ast
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runBoMTExcept (initCollectState typeSupplyCount) (collectAST annotated)

            -- turn type constraints into substitutions using unify
            let sos     = SymTab.lookupKey Collect.KeyType (symTab collectState)
            let typeMap = Map.map (\(ObjType t) -> t) $ Map.fromList sos
            subs <- fmap fst $ runBoMTExcept typeMap (unify $ Map.toList $ collected collectState)
            ast' <- fmap snd $ runBoMTExcept (applySubs subs annotated) CleanUp.compile
            ast'' <- fmap fst $ runBoMTExcept () $ deAnnotate ast'
            return ast''


        inferDefaults :: BoM s m => ASTResolved -> m ASTResolved
        inferDefaults ast = do
            (annotated, typeSupplyCount) <- withErrorPrefix "annotate: " $
                runBoMTExcept 0 $ annotate ast
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runBoMTExcept (initCollectState typeSupplyCount) (collectAST annotated)

            -- turn type constraints into substitutions using unify
            let sos     = SymTab.lookupKey Collect.KeyType (symTab collectState)
            let typeMap = Map.map (\(ObjType t) -> t) $ Map.fromList sos
            subs <- fmap fst $ runBoMTExcept typeMap (unifyDefault $ Map.toList $ defaults collectState)

            -- apply substitutions to ast
            ast' <- fmap snd $ runBoMTExcept (applySubs subs annotated) CleanUp.compile
            ast'' <- fmap fst $ runBoMTExcept () $ deAnnotate ast'
            return ast''
