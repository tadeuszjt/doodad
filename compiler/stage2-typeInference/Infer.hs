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

data RunReturn = RunReturn ASTResolved Int
instance Eq RunReturn where -- simple class to use for runBoMUntilSameResult
    (RunReturn ast1 n1) == (RunReturn ast2 n2) = ast1 == ast2


-- Takes a resolved and annotated ast and inferes all types.
infer :: BoM s m => ASTResolved -> Bool -> m (ASTResolved, Int)
infer resolvedAST verbose = do 
    (ast, typeSupplyCount) <- withErrorPrefix "annotate: " $ runBoMTExcept 0 $ annotate resolvedAST
    runBoMUntilSameResult ast $ \ast' -> do 
        (RunReturn inferred typeSupplyCount', _)  <- runBoMUntilSameResult (RunReturn ast' typeSupplyCount) inferTypes
        (RunReturn defaulted _, _) <- runBoMUntilSameResult (RunReturn inferred typeSupplyCount') inferDefaults
        return defaulted
    where
        inferTypes :: BoM s m => RunReturn -> m RunReturn
        inferTypes (RunReturn ast typeSupplyCount) = do
            --liftIO $ putStrLn "inferTypes"
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runBoMTExcept (initCollectState typeSupplyCount) (collectAST ast)

            -- turn type constraints into substitutions using unify
            let sos     = SymTab.lookupKey Collect.KeyType (symTab collectState)
            let typeMap = Map.map (\(ObjType t) -> t) $ Map.fromList sos
            subs <- fmap fst $ runBoMTExcept typeMap (unify $ Map.toList $ collected collectState)
            --return $ (applySubs subs ast)
            ast' <- fmap snd $ runBoMTExcept (applySubs subs ast) CleanUp.compile
            return $ RunReturn ast' (typeSupply collectState)


        inferDefaults :: BoM s m => RunReturn -> m RunReturn
        inferDefaults (RunReturn ast typeSupplyCount) = do
            --liftIO $ putStrLn "inferDefaults"
            collectState <- fmap snd $ withErrorPrefix "collect: " $
                runBoMTExcept (initCollectState typeSupplyCount) (collectAST ast)

            -- turn type constraints into substitutions using unify
            let sos     = SymTab.lookupKey Collect.KeyType (symTab collectState)
            let typeMap = Map.map (\(ObjType t) -> t) $ Map.fromList sos
            subs <- fmap fst $ runBoMTExcept typeMap (unifyDefault $ Map.toList $ defaults collectState)

            -- apply substitutions to ast
            ast' <- fmap snd $ runBoMTExcept (applySubs subs ast) CleanUp.compile
            return $ RunReturn ast' (typeSupply collectState)
