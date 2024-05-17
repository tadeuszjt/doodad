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
import Monad
import Error
import Args
import AST
import Annotate
import Infer
import ASTResolved
import CBuilder as C
import CPretty as C
import CGenerate as C
import Lexer
import Compile as C
import COptimise as O
import Checker
import qualified ResolveAst
import qualified CombineAsts
import Preprocess

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
    state <- fmap snd $ runDoMExcept (initModulesState doodadPath) (buildModule True args modPath)

    let hDoodad   = joinPath [doodadPath, "include"]
    --let cDoodad   = joinPath [doodadPath, "include/doodad.c"]
    --let cFiles    = Map.elems (cFileMap state) ++ [cDoodad]
    let cFiles    = Map.elems (cFileMap state)
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
            exitCode <- rawSystem "gcc" $ ["-O3"] ++ ["-S"] ++ ["-I", hDoodad] ++ [cFile] ++ ["-o", asmPath]
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


getCanonicalModPath :: FilePath -> DoM Modules FilePath
getCanonicalModPath path = do
    doodadPath <- gets doodadPath
    let isRelative = isPrefixOf "../" path || isPrefixOf "./" path
    let modPath' = if isRelative then path else joinPath [doodadPath, path]
    liftIO (canonicalizePath modPath')


buildModule :: Bool -> Args -> FilePath -> DoM Modules ()
buildModule isMain args modPath = do
    doodadPath <- gets doodadPath
    absoluteModPath <- getCanonicalModPath modPath

    isCompiled <- Map.member absoluteModPath <$> gets moduleMap
    when (not isCompiled) $ do
        let modName = takeFileName absoluteModPath
        let modDirectory = takeDirectory absoluteModPath

        -- get files and parse asts
        files <- getSpecificModuleFiles args modName =<< getDoodadFilesInDirectory modDirectory
        file <- case files of
            [] -> fail ("module file not found in path: " ++ absoluteModPath)
            [x] -> return x
            xs  -> fail ("multiple matching module files found in path: " ++ absoluteModPath)

        astNoPre <- parse args file
        ast <- preprocess astNoPre


        when (isMain && printAst args) $ liftIO (S.prettyAST ast)

        -- read imports and compile imported modules first
        importPaths <- fmap (Set.toList . Set.fromList) $
            forM [fp | S.Import fp <- S.astImports ast] $ \importPath -> do
                getCanonicalModPath importPath
        mapM (buildModule False args) importPaths

        -- compile this module
        liftIO $ putStrLn ("compiling: " ++ absoluteModPath)


        -- unify asts and resolve symbols
        astImports <- forM importPaths $ \importPath -> do
            resm <- Map.lookup importPath <$> gets moduleMap
            unless (isJust resm) (error $ show importPath ++ " not in module map")
            return (fromJust resm)

        when (verbose args) $ liftIO $ putStrLn "resolving symbols..."
        (astResolved', supply) <- ResolveAst.resolveAst ast astImports
        astCombined' <- CombineAsts.combineAsts (astResolved', supply) astImports
        --liftIO $ prettyAST astResolved'


        let astResolved = astCombined'

        when (isMain && printAstResolved args) $ liftIO (prettyASTResolved astResolved)


        -- infer ast types
        when (verbose args) $ liftIO $ putStrLn "inferring types..."
        (astFinal) <- withErrorPrefix "infer: " $
            infer astResolved (printAstAnnotated args) (verbose args)

        when (isMain && printAstFinal args ) $ liftIO (prettyASTResolved astFinal)

        -- check ast for memory/type violations
        --withErrorPrefix "checker: " (runASTChecker astFinal)

        -- build C ast from final ast
        when (verbose args) $ liftIO $ putStrLn "generating C file..."
        res <- generateAst astFinal
        (((), something), cBuilderState) <- case res of
            Right x -> return x
            Left e -> throwError e

        -- optimise C builder state
        let includePaths = includes astFinal
        finalBuilderState <- if Args.optimise args then do
            (((), n), cBuilderStateOptimised) <- runDoMExcept cBuilderState $ do
                runDoMUntilSameState O.optimise
            when (verbose args) $ do
                liftIO $ putStrLn ("ran:       " ++ show n ++ " optimisation passes")
            return cBuilderStateOptimised
        else return cBuilderState

        -- write builder state to C file
        cFilePath <- liftIO $ writeSystemTempFile (modName ++ ".c") ""
        modify $ \s -> s { cFileMap = Map.insert absoluteModPath cFilePath (cFileMap s) }
        cHandle <- liftIO $ openFile cFilePath WriteMode
        void $ runDoMExcept (initCPrettyState cHandle finalBuilderState) (cPretty includePaths)
        void $ liftIO $ hClose cHandle
        count <- liftIO $ length . lines <$> readFile cFilePath
        when (verbose args) $ liftIO $ putStrLn $ "wrote c:   " ++ cFilePath ++ " LOC:" ++ show count
        
        let astCompiled = C.astResolved something
        modify $ \s -> s { moduleMap = Map.insert absoluteModPath astCompiled (moduleMap s) }
