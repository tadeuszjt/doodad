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
import qualified Collect as C


mapType :: (Type -> Type) -> Type -> Type
mapType f typ = case typ of
    t | isSimple t  -> f t
    T.Type id    -> f typ
    T.Void       -> f typ
    T.Typedef _  -> f typ
    T.Tuple ts   -> f $ T.Tuple [mapType f t | t <- ts]
    T.Table ts   -> f $ T.Table [mapType f t | t <- ts]
    T.Func ts rt -> f $ T.Func  [mapType f t | t <- ts] (mapType f rt)
    _ -> error $ show typ



data InferState =
    InferState
        { imports      :: Map.Map ModuleName InferState
        , exprIdSupply :: Int
        , typeIdSupply :: Int
        , symIdSupply  :: Int
        , curRetty     :: Type
        }
    deriving (Show)


initInferState imp = InferState
    { imports      = imp
    , exprIdSupply = 0
    , typeIdSupply = 0
    , symIdSupply  = 0
    , curRetty     = Void
    }


data RunInferState
    = RunInferState
        { modInferMap :: Map.Map FilePath (AST, C.SymTab)
        }


initRunInferState = RunInferState { modInferMap = Map.empty }


runTypeInference :: BoM s m => AST -> Map.Map ModuleName C.SymTab -> m AST
runTypeInference annotatedAST imports = do
    (_, state) <- runBoMTExcept (C.initCollectState imports) (C.collectAST annotatedAST)
    let subs = unify $ C.collected state
    return (apply subs annotatedAST)


runModInfer :: BoM RunInferState m => FilePath -> Set.Set FilePath -> m (AST, C.SymTab)
runModInfer modPath pathsVisited = do
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
                (_, symTab) <- runModInfer importPath (Set.insert path pathsVisited)
                return (takeFileName importPath, symTab)

            annotatedAST <- fmap fst $ withFiles files $ runBoMTExcept 0 (annotateAST combinedAST)
            (_, state) <- withFiles files $
                runBoMTExcept (C.initCollectState importMap) (C.collectAST annotatedAST)

            liftIO $ SymTab.prettySymTab (C.symTab state)
            modify $ \s -> s { modInferMap = Map.insert path (annotatedAST, C.symTab state) (modInferMap s) }


            ast1 <- withFiles files $ runTypeInference annotatedAST importMap
            ast2 <- withFiles files $ runTypeInference ast1 importMap
            ast3 <- withFiles files $ runTypeInference ast2 importMap

            liftIO $ prettyAST (ast3)
            return (ast3, C.symTab state)




