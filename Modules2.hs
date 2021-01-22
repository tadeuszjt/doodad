{-# LANGUAGE FlexibleContexts #-}
module Modules2 where

import System.Environment
import System.Directory
import Control.Monad.IO.Class
import Control.Monad.Except hiding (void, fail)
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified Parser as P
import qualified Lexer as L
import Monad
import Error

data Module
    = Module
        { filepaths :: Set.Set String
        , result    :: Maybe ()
        }


data Modules
    = Modules
        { modMap :: Map.Map String Module
        }


initModulesState
    = Modules
        { modMap = Map.empty
        }


runFile :: BoM Modules m => String -> m ()
runFile filename = do
    source <- liftIO (readFile filename)
    ast <- case (P.parse filename source) of
        Left err  -> throwError err
        Right ast -> return ast

    forM_ (S.astImports ast) $ \imprt -> do
        files <- liftIO $ getBoFilesInDirectory "lib"
        liftIO $ putStrLn $ show files
        return ()

    
    liftIO (S.prettyAST "" ast)

    
    return ()

    where
        getBoFilesInDirectory :: FilePath -> IO [FilePath]
        getBoFilesInDirectory dir = do
            list <- listDirectory dir
            return [ dir ++ "/" ++ f | f <- list, isSuffixOf ".bo" f ]

