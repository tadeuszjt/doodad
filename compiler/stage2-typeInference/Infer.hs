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
infer :: ASTResolved -> Bool -> Bool -> DoM s (ASTResolved, Int)
infer ast printAnnotated verbose = do 
    runDoMUntilSameResult ast $ \ast' -> do 
        --when verbose $ liftIO $ putStrLn $ "inferring..."
        (inferred, inferCount) <- runDoMUntilSameResult ast' inferTypes
        when verbose $ liftIO $ putStrLn ("inferred types " ++ show inferCount ++ " times")
        (defaulted, defaultCount) <- runDoMUntilSameResult inferred inferDefaults
        when verbose $ liftIO $ putStrLn ("inferred defaults " ++ show defaultCount ++ " times")
        return defaulted
    where
        inferTypes :: ASTResolved -> DoM s ASTResolved
        inferTypes ast = do
            (annotated, _) <- withErrorPrefix "annotate: " $ runDoMExcept 0 $ annotate ast
            when printAnnotated $ do
                liftIO $ putStrLn ""
                liftIO $ putStrLn "annotated AST:"
                liftIO $ prettyASTResolved annotated
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runDoMExcept (initCollectState annotated) (collectAST verbose annotated)

            -- turn type constraints into substitutions using unify
            subs <- fmap fst $ runDoMExcept ast (unify $ Map.toList $ collected collectState)
            annotated' <- applySubs2 subs annotated
            ast' <- fmap snd $ runDoMExcept annotated' (CleanUp.compile verbose)
            ast'' <- fmap fst $ runDoMExcept () $ deAnnotate ast'
            return ast''


        inferDefaults :: ASTResolved -> DoM s ASTResolved
        inferDefaults ast = do
            (annotated, _) <- withErrorPrefix "annotate: " $ runDoMExcept 0 $ annotate ast
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runDoMExcept (initCollectState annotated) (collectAST verbose annotated)

            -- apply substitutions to ast
            subs <- fmap fst $ runDoMExcept ast (unifyDefault $ Map.toList $ defaults collectState)
            annotated' <- applySubs subs annotated
            ast' <- fmap snd $ runDoMExcept annotated' (CleanUp.compile verbose)
            ast'' <- fmap fst $ runDoMExcept () $ deAnnotate ast'
            return ast''
