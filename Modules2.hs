{-# LANGUAGE FlexibleContexts #-}
module Modules2 where

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
    runMod "main"



runMod :: BoM Modules m => String -> m ()
runMod modPath = do
    liftIO $ putStrLn ("running mod: " ++ modPath) 

    (name, mpath) <- parseImport modPath

    files <- liftIO $ getSpecificModuleFiles name =<< getBoFilesInDirectory (maybe "." id mpath)
    when (null files) $ fail ("no files for: " ++ modPath)
    forM_ files $ \file -> do
        liftIO $ putStrLn (name ++ " using file: " ++ file)

    asts <- forM files $ \file ->
        P.parse file =<< liftIO (readFile file) 

    combinedAST <- combineASTs asts
    forM_ (S.astImports combinedAST) $ \imprt -> do
        liftIO $ putStrLn (modPath ++ " importing " ++ imprt)
        let path = maybe "" (++ "/") mpath
        runMod (path ++ imprt)


    res <- runBoMT initFlattenState (flattenAST combinedAST)
    flat <- case res of
        Left err              -> throwError err
        Right ((), flatState) -> return flatState

    return ()





parseImport :: Monad m => String -> m (String, Maybe String)
parseImport str = do
    let name = reverse $ takeWhile (/= '/') (reverse str)
    let mstrip = stripPrefix (reverse name) (reverse str)
    case mstrip of
        Nothing      -> return (name, Nothing)
        Just ""      -> return (name, Nothing)
        Just ('/':p) -> return (name, Just $ reverse p)



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


    
    

