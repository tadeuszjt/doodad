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
import CleanUp
import Type

-- Takes a resolved and annotated ast and inferes all types.
infer :: BoM s m => ASTResolved -> Bool -> Bool -> m (ASTResolved, Int)
infer ast printAnnotated verbose = do 
    runBoMUntilSameResult ast $ \ast' -> do 
        when verbose $ liftIO $ putStrLn $ "inferring..."
        (inferred, _) <- runBoMUntilSameResult ast' inferTypes
        (defaulted, _) <- runBoMUntilSameResult inferred inferDefaults
        return defaulted
    where
        inferTypes :: BoM s m => ASTResolved -> m ASTResolved
        inferTypes ast = do
            when verbose $ liftIO $ putStrLn $ "inferring types..."
            (annotated, _) <- withErrorPrefix "annotate: " $ runBoMTExcept 0 $ annotate ast
            when printAnnotated $ do
                liftIO $ putStrLn ""
                liftIO $ putStrLn "annotated AST:"
                liftIO $ prettyASTResolved annotated
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runBoMTExcept (initCollectState annotated) (collectAST verbose annotated)

            -- turn type constraints into substitutions using unify
            subs <- fmap fst $ runBoMTExcept ast (unify $ Map.toList $ collected collectState)
            annotated' <- applySubs subs annotated
            ast' <- fmap snd $ runBoMTExcept annotated' (CleanUp.compile verbose)
            ast'' <- fmap fst $ runBoMTExcept () $ deAnnotate ast'
            return ast''


        inferDefaults :: BoM s m => ASTResolved -> m ASTResolved
        inferDefaults ast = do
            when verbose $ liftIO $ putStrLn $ "inferring defaults..."
            (annotated, _) <- withErrorPrefix "annotate: " $ runBoMTExcept 0 $ annotate ast
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runBoMTExcept (initCollectState annotated) (collectAST verbose annotated)

            -- apply substitutions to ast
            subs <- fmap fst $ runBoMTExcept ast (unifyDefault $ Map.toList $ defaults collectState)
            annotated' <- applySubs subs annotated
            ast' <- fmap snd $ runBoMTExcept annotated' (CleanUp.compile verbose)
            ast'' <- fmap fst $ runBoMTExcept () $ deAnnotate ast'
            return ast''
