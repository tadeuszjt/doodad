{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Modules where

import System.Process
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Environment
import System.Directory
import Control.Monad.State
--import Control.Monad.IO.Class
import Control.Monad.Except hiding (void, fail)
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified Parser as P
import Args
import ASTResolved
import CPretty as C
import Lexer
import Compile as C
import qualified ResolveAst
import qualified CombineAsts
import Preprocess
import IrGenerate
import IrGenerateDestroy
import IrContextHeaderPass
import IrContextCallPass
import SemanticReferenceCheck
import Error

-- Modules are groups of .doo files with a module name header
-- lang/lexer.doo: lexer module


newtype Modules a = Modules
    { unModules :: StateT ModulesState (ExceptT Error IO) a }
    deriving (Functor, Applicative, Monad, MonadState ModulesState, MonadError Error, MonadIO)


runModules :: ModulesState -> Modules a -> IO (Either Error (a, ModulesState))
runModules modulesState f
    = runExceptT $ runStateT (unModules f) modulesState


data ModulesState
    = ModulesState
        { moduleMap :: Map.Map FilePath ASTResolved
        , cFileMap  :: Map.Map FilePath FilePath
        , doodadPath :: FilePath
        }


initModulesState doodadPath = ModulesState
    { moduleMap = Map.empty
    , cFileMap  = Map.empty
    , doodadPath = doodadPath
    }


getDoodadFilesInDirectory :: FilePath -> IO [FilePath]
getDoodadFilesInDirectory dir = do
    list <- liftIO (listDirectory dir)
    return [ dir ++ "/" ++ f | f <- list, isSuffixOf ".doo" f ]


readModuleName :: FilePath -> IO (Maybe String)
readModuleName filePath = do 
    firstLine <- liftIO $ do
        handle <- openFile filePath ReadMode
        line <- hGetLine handle
        hClose handle
        return (dropWhile isSpace line)

    let modStr = takeWhile isAlpha firstLine
    let nameStr = dropWhile isSpace (dropWhile isAlpha firstLine)
    case modStr of 
        "module" -> return (Just nameStr)
        _ -> return Nothing


getSpecificModuleFiles :: Args -> String -> [FilePath] -> IO [FilePath]
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
parse :: Args -> FilePath -> IO S.AST
parse args file = do
    newTokens <- liftIO $ lexFile (printTokens args) file
    when (printTokens args) $ do
        liftIO $ mapM_ (putStrLn . show) newTokens
    case runExcept (P.parse newTokens) of
        Right ast -> return ast
        Left err  -> printError err >> fail ""


buildBinaryFromModule :: Args -> FilePath -> IO ()
buildBinaryFromModule args modPath = do
    doodadPath <- getEnv "DOODAD_PATH"
    stateEither <- runModules (initModulesState doodadPath) (buildModule True args modPath)
    state <- case stateEither of
        Right (_, state) -> return state
        Left err         -> printError err >> fail ""

    let hDoodad   = joinPath [doodadPath, "include"]
    --let cDoodad   = joinPath [doodadPath, "include/doodad.c"]
    --let cFiles    = Map.elems (cFileMap state) ++ [cDoodad]
    let cFiles    = Map.elems (cFileMap state)
    let binFile   = takeFileName modPath
    let linkPaths = Set.toList $ Set.unions (map links $ Map.elems $ moduleMap state)

    forM_ linkPaths $ \path -> do
        putStrLn $ "linking '" ++ path ++ "'"

    when (printC args) $ do
        forM_ (cFiles) $ \file -> do
            putStrLn =<< readFile file

    when (printAssembly args) $ do
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


    exitCode <- rawSystem "gcc" $ -- ["-pg"]
        ["-I", hDoodad] ++ cFiles ++ map ("-l" ++) linkPaths ++ ["-o", binFile]
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure s -> fail $ "gcc failed: " ++ (show s)

    putStrLn $ "wrote bin: " ++ binFile

    forM_ (Map.toList $ cFileMap state) $ \(_, cFile) -> do
        removeFile cFile


getCanonicalModPath :: FilePath -> Modules FilePath
getCanonicalModPath path = do
    doodadPath <- gets doodadPath
    let isRelative = isPrefixOf "../" path || isPrefixOf "./" path
    let modPath' = if isRelative then path else joinPath [doodadPath, path]
    liftIO (canonicalizePath modPath')


buildModule :: Bool -> Args -> FilePath -> Modules ()
buildModule isMain args modPath = do
    --doodadPath <- gets doodadPath
    absoluteModPath <- getCanonicalModPath modPath

    isCompiled <- Map.member absoluteModPath <$> gets moduleMap
    when (not isCompiled) $ do
        let modName = takeFileName absoluteModPath
        let modDirectory = takeDirectory absoluteModPath

        -- get files and parse asts
        files <- liftIO $ getSpecificModuleFiles args modName =<< getDoodadFilesInDirectory modDirectory
        file <- case files of
            [] -> throwError $ ErrorStr ("module file not found in path: " ++ absoluteModPath)
            [x] -> return x
            _  -> throwError $ ErrorStr ("multiple matching module files found in path: " ++ absoluteModPath)

        ast_ <- liftIO (parse args file)
        when (printAst args) $ liftIO (S.prettyAST ast_)

        -- read imports and compile imported modules first
        imports <- fmap (Set.toList . Set.fromList) $
            forM [ (stmt, path) | stmt@(S.Import _ _ path _) <- S.astImports ast_] $ \(stmt, path)-> do
                path' <- getCanonicalModPath path
                return (stmt, path')

        let ast = preprocess ast_
        mapM_ (buildModule False args) (map snd imports)

        -- compile this module
        liftIO $ putStrLn ("compiling: " ++ absoluteModPath)


        -- unify asts and resolve symbols
        astImports <- forM imports $ \(stmt, canonPath) -> do
            resm <- Map.lookup canonPath <$> gets moduleMap
            unless (isJust resm) (error $ show canonPath ++ " not in module map")
            return (stmt, fromJust resm)

        when (verbose args) $ liftIO $ putStrLn "resolving symbols..."
        (astResolved', supply) <- liftEither (ResolveAst.resolveAst ast astImports)

        --when (printAstResolved args) $ liftIO (prettyAST astResolved')

        astFinal <- liftEither (CombineAsts.combineAsts astResolved' supply astImports)
        when (isMain && printAstFinal args) $ liftIO (prettyASTResolved astFinal)

        astGenerated <- liftEither $ fmap snd $ runExcept $ (flip runStateT) astFinal $ do
            irGenerateAst
            --irGenerateDestroyPass
            irContextHeaderPass
            irContextCallPass

        --liftEither $ runExcept (semanticReferenceCheck astGenerated)

        when (isMain && printIr args) $ liftIO (printAstIr astGenerated)

        -- build C ast from final ast
        when (verbose args) $ liftIO $ putStrLn "generating C file..."
        cBuilderState <- fmap snd $ liftEither (generateAst astGenerated)

        -- optimise C builder state
        let includePaths = includes astGenerated
        let finalBuilderState = cBuilderState

        -- write builder state to C file
        cFilePath <- liftIO $ writeSystemTempFile (modName ++ ".c") ""
        modify $ \s -> s { cFileMap = Map.insert absoluteModPath cFilePath (cFileMap s) }
        cHandle <- liftIO $ openFile cFilePath WriteMode
        runCPretty finalBuilderState cHandle (cPretty includePaths)
        void $ liftIO $ hClose cHandle
        count <- liftIO $ length . lines <$> readFile cFilePath
        when (verbose args) $ liftIO $ putStrLn $ "wrote c:   " ++ cFilePath ++ " LOC:" ++ show count
        
        modify $ \s -> s { moduleMap = Map.insert absoluteModPath astGenerated (moduleMap s) }
