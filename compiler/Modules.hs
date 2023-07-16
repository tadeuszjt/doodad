{-# LANGUAGE FlexibleContexts #-}
module Modules where

import System.Process
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
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

--import Lexer
import qualified AST as S
import qualified Parser as P
import Flatten
import Monad
import Error
import Args
import AST
import Annotate
import Infer
import Collect as C
import qualified Resolve as R
import qualified SymTab
import States
import Resolve2
import CBuilder as C
import CPretty as C
import CGenerate as C
import Lexer

-- Modules are groups of .doo files with a module name header
-- lang/lexer.doo: lexer module


data Modules
    = Modules
        { moduleMap :: Map.Map FilePath ResolvedAst
        , cFileMap  :: Map.Map FilePath FilePath
        }


initModulesState 
    = Modules
        { moduleMap = Map.empty
        , cFileMap  = Map.empty
        }


getDoodadFilesInDirectory :: BoM s m => FilePath -> m [FilePath]
getDoodadFilesInDirectory dir = do
    list <- liftIO (listDirectory dir)
    return [ dir ++ "/" ++ f | f <- list, isSuffixOf ".doo" f ]



readModuleName :: BoM s m => FilePath -> m (Maybe String)
readModuleName filePath = do 
    src <- liftIO (readFile filePath) 
    let start = dropWhile isSpace src
    let modStr = takeWhile isAlpha start
    let nameStr = takeWhile (\c -> isAlpha c || isDigit c) $ dropWhile isSpace $ dropWhile isAlpha start
    case modStr of 
        "module" -> return (Just nameStr)
        _ -> return Nothing


getSpecificModuleFiles :: BoM s m => Args -> String -> [FilePath] -> m [FilePath]
getSpecificModuleFiles args name []     = return []
getSpecificModuleFiles args name (f:fs) = do
    source <- liftIO (readFile f)
    namem <- readModuleName f
    if namem == (Just name) then
        (f:) <$> getSpecificModuleFiles args name fs
    else
        getSpecificModuleFiles args name fs


-- parse a file into an AST.
-- Throw an error on failure.
parse :: BoM s m => Args -> FilePath -> m S.AST
parse args file = do
    newTokens <- liftIO $ lexFile (printTokens args) file
    when (printTokens args) $ do
        liftIO $ mapM_ (putStrLn . show) newTokens
    P.parse newTokens


runMod :: BoM s m => Args -> FilePath -> m ()
runMod args modPath = do
    ((), state) <- runBoMTExcept (initModulesState) $ runMod' args modPath
    let cFiles = Map.elems (cFileMap state) ++ ["include/doodad.c"] ++ ["include/main.c"]
    let binFile = takeFileName modPath

    exitCode <- liftIO $ rawSystem "gcc" $ ["-I", "include"] ++ cFiles ++ ["-lgc", "-o", binFile]
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure s -> fail $ "gcc failed: " ++ (show s)

    forM_ (Map.toList $ cFileMap state) $ \(_, cFile) -> do
        liftIO $ removeFile cFile



runMod' :: BoM Modules m => Args -> FilePath -> m ()
runMod' args modPath = do
    debug "running"
    absolute <- liftIO $ canonicalizePath modPath
    debug $ "absolute path: " ++ show absolute
    isCompiled <- Map.member absolute <$> gets moduleMap
    when (not isCompiled) $ compilePath absolute
    where
        -- path will be in the form "dir1/dirn/modname"
        compilePath :: BoM Modules m => FilePath -> m ()
        compilePath path = do
            debug "compilePath"
            let modName = takeFileName path
            let modDirectory = takeDirectory path

            -- get files and create combined AST
            files <- getSpecificModuleFiles args modName =<< getDoodadFilesInDirectory modDirectory
            assert (not $ null files) ("no files for: " ++ path)
            forM_ files $ debug . ("using file: " ++) 


            asts <- mapM (parse args) files
            when (printAst args) $ do
                forM_ asts $ \ast ->
                    liftIO $ S.prettyAST ast


            importPaths <- forM [fp | S.Import fp <- concat $ map S.astImports asts] $ \importPath ->
                liftIO $ canonicalizePath $ joinPath [modDirectory, importPath]
            let importModNames = map takeFileName importPaths
            assert (length importModNames == length (Set.fromList importModNames)) $
                fail "import name collision"
            forM_ importPaths $ debug . ("importing: " ++)
            mapM_ (runMod' args) importPaths

            debug "loading irGenImports"
            astImports <- forM importPaths $ \path -> do
                resm <- Map.lookup path <$> gets moduleMap
                assert (isJust resm) $ show path ++ " not in module map"
                return $ fromJust resm
            (resolvedAST, _) <- R.resolveAsts asts astImports
            Flatten.checkTypeDefs (typeDefs resolvedAST)
            when (printAstResolved args) $ liftIO $ prettyResolvedAst resolvedAST

            debug "annotating ast"
            annotatedAST <- fmap fst $ withErrorPrefix "annotate: " $
                runBoMTExcept 0 $ annotate resolvedAST
            when (printAstAnnotated args) $ liftIO $ prettyResolvedAst annotatedAST
            astInferred <- withErrorPrefix "infer: " $ infer annotatedAST (verbose args)


            debug "resolve2"
            ((), resolved2) <- withErrorPrefix "resolve2: " $ runBoMTExcept astInferred Resolve2.compile
            modify $ \s -> s { moduleMap = Map.insert path (resolved2) (moduleMap s) }
            when (printAstFinal args) $ liftIO $ prettyResolvedAst resolved2

            -- create 'build' directory
            buildDir <- liftIO $ canonicalizePath $ "build"
            liftIO $ createDirectoryIfMissing True buildDir

            cFilePath <- liftIO $ writeSystemTempFile (modName ++ ".c") ""
            modify $ \s -> s { cFileMap = Map.insert path cFilePath (cFileMap s) }

            liftIO $ putStrLn $ "writing: " ++ cFilePath
            cHandle <- liftIO $ openFile cFilePath WriteMode
            (_, cBuilderState) <- runGenerateT (C.initGenerateState) (C.initBuilderState modName) (generate resolved2)
            _ <- runBoMTExcept (initCPrettyState cHandle cBuilderState) cPretty
            liftIO $ hClose cHandle

            return ()

        debug str =
            if verbose args
            then liftIO $ putStrLn (takeFileName modPath ++ " -> " ++ str)
            else return ()

