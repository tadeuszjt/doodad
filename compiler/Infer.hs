{-# LANGUAGE FlexibleContexts #-}
module Infer where

import System.FilePath
import Control.Monad.State
import Control.Monad.Except hiding (void, fail)
import Data.Maybe
import Data.Word
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified SymTab
import Type as T
import AST
import Error
import Monad
import Modules
import Flatten hiding (imports)
import Annotate
import Unify
import Args
import qualified Collect as C


data RunInferState
    = RunInferState
        { modInferMap :: Map.Map FilePath (AST, C.SymTab)
        }


initRunInferState = RunInferState { modInferMap = Map.empty }


runModInfer :: BoM RunInferState m => Args -> FilePath -> Set.Set FilePath -> m (AST, C.SymTab)
runModInfer args modPath pathsVisited = do
    path <- checkAndNormalisePath modPath
    assert (not $ Set.member path pathsVisited) ("importing: " ++ path ++ " forms a cycle")
    resm <- Map.lookup path <$> gets modInferMap
    maybe (inferPath path) (return) resm
    where
        inferPath :: BoM RunInferState m => FilePath -> m (AST, C.SymTab)
        inferPath path = do
            let modName      = takeFileName path
            let modDirectory = takeDirectory path
            files <- getSpecificModuleFiles modName =<< getBoFilesInDirectory modDirectory
            assert (not $ null files) ("no files for: " ++ path)

            combinedAST <- combineASTs =<< zipWithM parse [0..] files
            importPaths <- forM (AST.astImports combinedAST) $ \importPath ->
                checkAndNormalisePath $ joinPath [modDirectory, importPath]

            let importNames = map takeFileName importPaths
            assert (length importNames == length (Set.fromList importNames)) "import name collision"

            importMap <- fmap Map.fromList $ forM importPaths $ \importPath -> do
                (_, symTab) <- runModInfer args importPath (Set.insert path pathsVisited)
                return (takeFileName importPath, symTab)

            annotatedAST <- fmap fst $ withFiles files $ runBoMTExcept 0 (annotateAST combinedAST)
            (ast, symTab) <- withFiles files $ runTypeInference args annotatedAST importMap

            liftIO $ SymTab.prettySymTab symTab
            modify $ \s -> s { modInferMap = Map.insert path (ast, symTab) (modInferMap s) }

            liftIO $ prettyAST ast
            return (ast, symTab)




