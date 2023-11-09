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
import TupleDeleter

-- Modules are groups of .doo files with a module name header
-- lang/lexer.doo: lexer module


data Modules
    = Modules
        { moduleMap :: Map.Map FilePath ASTResolved
        , cFileMap  :: Map.Map FilePath FilePath
        , doodadPath :: FilePath
        }


initModulesState doodadPath
    = Modules
        { moduleMap = Map.empty
        , cFileMap  = Map.empty
        , doodadPath = doodadPath
        }


getDoodadFilesInDirectory :: FilePath -> DoM s [FilePath]
getDoodadFilesInDirectory dir = do
    list <- liftIO (listDirectory dir)
    return [ dir ++ "/" ++ f | f <- list, isSuffixOf ".doo" f ]


readModuleName :: FilePath -> DoM s (Maybe String)
readModuleName filePath = do 
    src <- liftIO (readFile filePath) 
    let start = dropWhile isSpace src
    let modStr = takeWhile isAlpha start
    let nameStr = takeWhile (\c -> isAlpha c || isDigit c) $ dropWhile isSpace $ dropWhile isAlpha start
    case modStr of 
        "module" -> return (Just nameStr)
        _ -> return Nothing


getSpecificModuleFiles :: Args -> String -> [FilePath] -> DoM s [FilePath]
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
parse :: Args -> FilePath -> DoM s S.AST
parse args file = do
    newTokens <- liftIO $ lexFile (printTokens args) file
    when (printTokens args) $ do
        liftIO $ mapM_ (putStrLn . show) newTokens
    P.parse newTokens


buildBinaryFromModule :: Args -> FilePath -> DoM s ()
buildBinaryFromModule args modPath = do
    doodadPath <- liftIO $ getEnv "DOODAD_PATH"
    state <- fmap snd $ runDoMExcept (initModulesState doodadPath) (buildModule args modPath)

    let hDoodad   = joinPath [doodadPath, "include"]
    let cDoodad   = joinPath [doodadPath, "include/doodad.c"]
    let cFiles    = Map.elems (cFileMap state) ++ [cDoodad]
    let binFile   = takeFileName modPath
    let linkPaths = Set.toList $ Set.unions (map links $ Map.elems $ moduleMap state)

    forM_ linkPaths $ \path -> do
        liftIO $ putStrLn $ "linking '" ++ path ++ "'"

    when (printC args) $ do
        forM_ (reverse cFiles) $ \file -> do
            liftIO $ putStrLn =<< readFile file

    when (printAssembly args) $ liftIO $ do
        locs <- forM cFiles $ \cFile -> do
            let asmPath = dropExtension cFile <.> ".s"
            exitCode <- rawSystem "gcc" $
                ["-S"] ++ ["-I", hDoodad] ++ [cFile] ++ ["-o", asmPath]
            case exitCode of
                ExitSuccess -> return ()
                ExitFailure s -> fail $ "gcc failed: " ++ show s

            putStrLn ""
            contents <- readFile asmPath
            let loc = length (lines contents)
            putStrLn contents
            removeFile asmPath
            return (loc, cFile)
        forM_ locs $ \(loc, cFile) -> putStrLn $ show loc ++ " lines of ASM for: " ++ show cFile


    exitCode <- liftIO $ rawSystem "gcc" $
        ["-I", hDoodad] ++ cFiles ++ ["-lgc"] ++ map ("-l" ++) linkPaths ++ ["-o", binFile]
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure s -> fail $ "gcc failed: " ++ (show s)

    liftIO $ putStrLn $ "wrote bin: " ++ binFile

    forM_ (Map.toList $ cFileMap state) $ \(_, cFile) -> do
        liftIO $ removeFile cFile



buildModule :: Args -> FilePath -> DoM Modules ()
buildModule args modPath = do
    doodadPath <- gets doodadPath
    let isRelative = isPrefixOf "../" modPath || isPrefixOf "./" modPath
    let modPath' = if isRelative then modPath else joinPath [doodadPath, modPath]
    absoluteModPath <- liftIO $ canonicalizePath modPath'

    isCompiled <- Map.member absoluteModPath <$> gets moduleMap
    when (not isCompiled) $ do
        let modName = takeFileName absoluteModPath
        let modDirectory = takeDirectory absoluteModPath

        -- get files and parse asts
        files <- getSpecificModuleFiles args modName =<< getDoodadFilesInDirectory modDirectory
        assert (not $ null files) ("no files for: " ++ absoluteModPath)
        asts <- mapM (parse args) files
        when (printAst args) $ mapM_ (liftIO . S.prettyAST) asts

        -- read imports and compile imported modules first
        importPaths <- fmap (Set.toList . Set.fromList) $
            forM [fp | S.Import fp <- concat $ map S.astImports asts] $ \importPath -> do
                let isRelative = isPrefixOf "../" importPath || isPrefixOf "./" importPath
                let importPath' = joinPath (if isRelative then [modDirectory, importPath] else [doodadPath, importPath])
                liftIO $ canonicalizePath importPath'
        mapM (buildModule args) importPaths

        -- compile this module
        liftIO $ putStrLn $ "compiling: " ++ absoluteModPath


        -- unify asts and resolve symbols
        astImports <- forM importPaths $ \importPath -> do
            resm <- Map.lookup importPath <$> gets moduleMap
            assert (isJust resm) $ show importPath ++ " not in module map"
            return $ fromJust resm
        astResolved' <- fmap (fst . fst) $ runDoMExcept () (R.resolveAsts asts astImports)
        --Flatten.checkTypeDefs (typeDefs astResolved)
        when (printAstResolved args) $ liftIO $ prettyASTResolved astResolved'

        -- remove spurious tuples
        astResolved <- fmap snd $ runDoMExcept astResolved' deleteSingleTuples

        -- infer ast types
        (astFinal, inferCount) <- fmap fst $ runDoMExcept () $ withErrorPrefix "infer: " $
            infer astResolved (printAstAnnotated args) (verbose args)
        liftIO $ putStrLn $ "ran:       " ++ show inferCount ++ " type inference passes"
        when (printAstFinal args)    $ liftIO $ prettyASTResolved astFinal
        modify $ \s -> s { moduleMap = Map.insert absoluteModPath astFinal (moduleMap s) }

        -- build C ast from final ast
        res <- runGenerateT
            (C.initGenerateState modName) (C.initBuilderState modName) (generate astFinal)
        cBuilderState <- case res of
            Right x -> return (snd x)
            Left e -> throwError e

        -- optimise C builder state
        let includePaths = includes astFinal
        finalBuilderState <- if Args.optimise args then do
            (((), n), cBuilderStateOptimised) <- runDoMExcept cBuilderState $ do
                runDoMUntilSameState O.optimise
            liftIO $ putStrLn $ "ran:       " ++ show n ++ " optimisation passes"
            return cBuilderStateOptimised
        else return cBuilderState

        -- write builder state to C file
        cFilePath <- liftIO $ writeSystemTempFile (modName ++ ".c") ""
        modify $ \s -> s { cFileMap = Map.insert absoluteModPath cFilePath (cFileMap s) }
        cHandle <- liftIO $ openFile cFilePath WriteMode
        void $ runDoMExcept (initCPrettyState cHandle finalBuilderState) (cPretty includePaths)
        void $ liftIO $ hClose cHandle
        count <- liftIO $ length . lines <$> readFile cFilePath
        liftIO $ putStrLn $ "wrote c:   " ++ cFilePath ++ " LOC:" ++ show count
