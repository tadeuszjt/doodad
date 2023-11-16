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
import CleanUp

-- Infer takes an ast and recursively runs the type inference algorithms until it can no longer
-- make any changes to the ast.

infer :: ASTResolved -> Bool -> Bool -> DoM s (ASTResolved, Int)
infer ast printAnnotated verbose = runDoMUntilSameResult ast $ \ast -> do 
    --when verbose $ liftIO $ putStrLn $ "inferring..."
    (inferred, inferCount) <- runDoMUntilSameResult ast inferTypes
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
            fmap fst $ runDoMExcept () $ deAnnotate ast'


        inferDefaults :: ASTResolved -> DoM s ASTResolved
        inferDefaults ast = do
            (annotated, _) <- withErrorPrefix "annotate: " $ runDoMExcept 0 $ annotate ast
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runDoMExcept (initCollectState annotated) (collectAST verbose annotated)

            -- apply substitutions to ast
            subs <- fmap fst $ runDoMExcept ast (unifyDefault $ Map.toList $ defaults collectState)
            annotated' <- applySubs subs annotated
            ast' <- fmap snd $ runDoMExcept annotated' (CleanUp.compile verbose)
            fmap fst $ runDoMExcept () $ deAnnotate ast'
