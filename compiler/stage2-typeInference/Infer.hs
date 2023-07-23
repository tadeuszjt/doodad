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


runUntilNoChange :: Eq a => BoM s m => a -> (a -> m a) -> m a
runUntilNoChange a f = do
    a' <- f a
    if a == a' then return a
    else runUntilNoChange a' f


-- Takes a resolved and annotated ast and inferes all types.
infer :: BoM s m => ASTResolved -> Bool -> m ASTResolved
infer resolvedAST verbose = do 
    (ast, typeSupplyCount) <- withErrorPrefix "annotate: " $ runBoMTExcept 0 $ annotate resolvedAST
    runUntilNoChange ast $ \ast' -> do 
        inferred <- runUntilNoChange ast' (inferTypes typeSupplyCount)
        defaulted <- runUntilNoChange inferred (inferDefaults typeSupplyCount)
        return defaulted
    where
        inferTypes :: BoM s m => Int -> ASTResolved -> m ASTResolved
        inferTypes typeSupplyCount ast = do
            --liftIO $ putStrLn "inferTypes"
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runBoMTExcept (initCollectState typeSupplyCount) (collectAST ast)

            -- turn type constraints into substitutions using unify
            let sos     = SymTab.lookupKey Collect.KeyType (symTab collectState)
            let typeMap = Map.map (\(ObjType t) -> t) $ Map.fromList sos
            subs <- fmap fst $ runBoMTExcept typeMap (unify $ Map.toList $ collected collectState)

            -- apply substitutions to ast
            return (applySubs subs ast)


        inferDefaults :: BoM s m => Int -> ASTResolved -> m ASTResolved
        inferDefaults typeSupplyCount ast = do
            --liftIO $ putStrLn "inferDefaults"
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runBoMTExcept (initCollectState typeSupplyCount) (collectAST ast)

            -- turn type constraints into substitutions using unify
            let sos     = SymTab.lookupKey Collect.KeyType (symTab collectState)
            let typeMap = Map.map (\(ObjType t) -> t) $ Map.fromList sos
            subs <- fmap fst $ runBoMTExcept typeMap (unifyDefault $ Map.toList $ defaults collectState)

            -- apply substitutions to ast
            return (applySubs subs ast)
