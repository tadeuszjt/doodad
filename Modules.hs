{-# LANGUAGE FlexibleContexts #-}
module Modules where

import System.Environment
import System.Directory
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Except hiding (void, fail)
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified Parser as P
import qualified Lexer as L
import Flatten
import Monad
import Error
import JIT
import Compile
import CompileState
import Args


data Modules
    = Modules
        { modMap  :: Map.Map S.Path CompileState
        , session :: JIT.Session
        }


initModulesState session
    = Modules
        { modMap  = Map.empty
        , session = session
        }


showPath :: S.Path -> String
showPath path = concat (intersperse "/" path)


runMod :: BoM Modules m => Args -> S.Path -> m CompileState
runMod args modPath = do
    res <- fmap (Map.lookup modPath) (gets modMap)
    case res of
        Just state -> return state
        Nothing    -> do
            when (verbose args) $ liftIO $ putStrLn ("running: " ++ showPath modPath) 

            let modName = last modPath
            let modDir  = showPath (init modPath)

            let dir = if null modDir then "." else modDir
            files <- liftIO $ getSpecificModuleFiles modName =<< getBoFilesInDirectory dir
            when (files == []) $ fail ("no files for: " ++ showPath modPath)

            asts <- forM files $ \file -> do
                when (verbose args) $ liftIO $ putStrLn ("using file: " ++ file)
                P.parse file =<< liftIO (readFile file) 

            -- flatten asts
            combinedAST <- combineASTs asts
            imports <- fmap Map.fromList $ forM (S.astImports combinedAST) $ \importPath -> do
                when (verbose args) $ liftIO $ putStrLn (showPath modPath ++ " importing " ++ showPath importPath)
                state <- runMod args importPath
                return (importPath, state)

            flatRes <- runBoMT initFlattenState (flattenAST combinedAST)
            flat <- case flatRes of
                Left err              -> throwError err
                Right ((), flatState) -> return flatState

            -- compile and run
            session <- gets session
            state <- compileFlatState (JIT.context session) (JIT.dataLayout session) imports flat
            liftIO $ jitAndRun (definitions state) session True (verbose args)
            modify $ \s -> s { modMap = Map.insert modPath state (modMap s) }
            return state


getBoFilesInDirectory :: FilePath -> IO [FilePath]
getBoFilesInDirectory dir = do
    list <- listDirectory dir
    return [ dir ++ "/" ++ f | f <- list, isSuffixOf ".bo" f ]


getSpecificModuleFiles :: String -> [FilePath] -> IO [FilePath]
getSpecificModuleFiles name []     = return []
getSpecificModuleFiles name (f:fs) = do
    source <- readFile f
    ast <- case (P.parse f source) of
        Left err  -> error (show err)
        Right ast -> return ast

    if fromJust (S.astModuleName ast) == name then
        fmap (f :) (getSpecificModuleFiles name fs)
    else
        getSpecificModuleFiles name fs


    
    

