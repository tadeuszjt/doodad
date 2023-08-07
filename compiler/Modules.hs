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
import ASTResolved
import CBuilder as C
import CPretty as C
import CGenerate as C
import Lexer
import Compile as C
import COptimise as O

-- Modules are groups of .doo files with a module name header
-- lang/lexer.doo: lexer module


data Modules
    = Modules
        { moduleMap :: Map.Map FilePath ASTResolved
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


buildModule :: BoM s m => Args -> FilePath -> m ()
buildModule args modPath = do
    ((), state) <- runBoMTExcept (initModulesState) $ buildModule' args modPath
    let cFiles = Map.elems (cFileMap state) ++ ["include/doodad.c"]
    let binFile = takeFileName modPath
    let linkPaths = Set.toList $ Set.unions (map links $ Map.elems $ moduleMap state)

    forM_ linkPaths $ \path -> do
        liftIO $ putStrLn $ "linking '" ++ path ++ "'"

    when (printC args) $ do
        forM_ (reverse cFiles) $ \file -> do
            liftIO $ putStrLn =<< readFile file

    exitCode <- liftIO $ rawSystem "gcc" $
        ["-I", "include"] ++ cFiles ++ ["-lgc"] ++ map ("-l" ++) linkPaths ++ ["-o", binFile]
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure s -> fail $ "gcc failed: " ++ (show s)

    liftIO $ putStrLn $ "wrote bin: " ++ binFile

    forM_ (Map.toList $ cFileMap state) $ \(_, cFile) -> do
        liftIO $ removeFile cFile



buildModule' :: BoM Modules m => Args -> FilePath -> m ()
buildModule' args modPath = do
    absolute <- liftIO $ canonicalizePath modPath
    isCompiled <- Map.member absolute <$> gets moduleMap
    when (not isCompiled) $ compilePath absolute
    where
        -- path will be in the form "dir1/dirn/modname"
        compilePath :: BoM Modules m => FilePath -> m ()
        compilePath path = do
            let modName = takeFileName path
            let modDirectory = takeDirectory path

            -- get files and create combined AST
            files <- getSpecificModuleFiles args modName =<< getDoodadFilesInDirectory modDirectory
            assert (not $ null files) ("no files for: " ++ path)
            asts <- mapM (parse args) files
            when (printAst args) $ do
                forM_ asts $ \ast ->
                    liftIO $ S.prettyAST ast

            -- read imports and compile imported modules first
            importPaths <- forM [fp | S.Import fp <- concat $ map S.astImports asts] $ \importPath ->
                liftIO $ canonicalizePath $ joinPath [modDirectory, importPath]
            let importModNames = map takeFileName importPaths
            assert (length importModNames == length (Set.fromList importModNames)) $
                fail "import name collision"
            mapM_ (buildModule' args) importPaths


            -- compile this module
            liftIO $ putStrLn $ "compiling: " ++ path


            -- unify asts and resolve symbols
            astImports <- forM importPaths $ \path -> do
                resm <- Map.lookup path <$> gets moduleMap
                assert (isJust resm) $ show path ++ " not in module map"
                return $ fromJust resm
            astResolved <- fmap fst $ R.resolveAsts asts astImports
            Flatten.checkTypeDefs (typeDefs astResolved)
            when (printAstResolved args) $ liftIO $ prettyASTResolved astResolved

            -- infer ast types
            (astFinal, inferCount) <- withErrorPrefix "infer: " $ infer astResolved (verbose args)
            liftIO $ putStrLn $ "ran:       " ++ show inferCount ++ " type inference passes"
            when (printAstFinal args)    $ liftIO $ prettyASTResolved astFinal
            modify $ \s -> s { moduleMap = Map.insert path astFinal (moduleMap s) }


            -- build C ast from final ast
            res <- runGenerateT
                (C.initGenerateState modName) (C.initBuilderState modName) (generate astFinal)
            cBuilderState <- case res of
                Right x -> return (snd x)
                Left e -> throwError e



            -- optimise C builder state
            let includePaths = includes astFinal
            finalBuilderState <- if Args.optimise args then do
                (((), n), cBuilderStateOptimised) <- runBoMTExcept cBuilderState $ do
                    runBoMUntilSameState O.optimise
                liftIO $ putStrLn $ "ran:       " ++ show n ++ " optimisation passes"
                return cBuilderStateOptimised
            else return cBuilderState


            -- write builder state to C file
            cFilePath <- liftIO $ writeSystemTempFile (modName ++ ".c") ""
            modify $ \s -> s { cFileMap = Map.insert path cFilePath (cFileMap s) }
            cHandle <- liftIO $ openFile cFilePath WriteMode
            void $ runBoMTExcept (initCPrettyState cHandle finalBuilderState) (cPretty includePaths)
            void $ liftIO $ hClose cHandle
            liftIO $ putStrLn $ "wrote:     " ++ cFilePath
