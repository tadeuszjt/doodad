{-# LANGUAGE FlexibleContexts #-}
module Modules where

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
import qualified Lexer as L
import Flatten
import Monad
import Error
import JIT
import Compile
import State
import Args
import AST
import Annotate
import Interop
import Infer
import Collect as C
import qualified Resolve as R
import qualified SymTab

import Language.C
import Language.C.System.GCC
import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Pretty
import Language.C.Syntax.AST
import Text.PrettyPrint

-- Modules are groups of .bo files with a module name header
-- lang/lexer.bo: lexer module


data Modules
    = Modules
        { modMap  :: Map.Map FilePath CompileState
        , symTabMap :: Map.Map String C.SymTab
        , resolveSymTabMap :: Map.Map FilePath R.SymTab
        , session :: JIT.Session
        }


initModulesState session
    = Modules
        { modMap  = Map.empty
        , session = session
        , resolveSymTabMap = Map.empty
        , symTabMap = Map.empty
        }


checkAndNormalisePath :: BoM s m => FilePath -> m FilePath
checkAndNormalisePath path = do
    assert (isValid path) (show path ++ " invalid")
    processSplitPath (splitPath path)

    where
        processSplitPath :: BoM s m => [FilePath] -> m FilePath
        processSplitPath path = case path of
            [x]          -> checkValidModuleName x >> return x
            ("./":xs)    -> processSplitPath xs
            (x:"../":xs) -> checkValidDirName x >> processSplitPath xs
            (x:xs)       -> checkValidDirName x >> joinPath . (x:) . (:[]) <$> processSplitPath xs
            xs           -> fail ("Cannot process: " ++ joinPath xs)

        checkValidModuleName :: BoM s m => String -> m ()
        checkValidModuleName name = do
            assert (not $ null name) $ "Invalid module name"
            assert (isAlpha $ head name) $ "Module name: " ++ name ++ " must have alpha as first char"
            assert (all (== True) $ map isAlphaNum name) $ "Module name: " ++ name ++ " must be all alphanum"

        checkValidDirName :: BoM s m => String -> m ()
        checkValidDirName dirName = do
            let name = dropTrailingPathSeparator dirName
            assert (not $ null name) "Invalid directory name"
            assert (isAlpha $ head name) $ "Directory name: " ++ name ++ " must have alpha as first char"
            assert (all (== True) $ map isAlphaNum name) $ "Module name: " ++ name ++ " must be all alphanum"


getBoFilesInDirectory :: BoM s m => FilePath -> m [FilePath]
getBoFilesInDirectory dir = do
    list <- liftIO (listDirectory dir)
    return [ dir ++ "/" ++ f | f <- list, isSuffixOf ".bo" f ]


getSpecificModuleFiles :: BoM s m => String -> [FilePath] -> m [FilePath]
getSpecificModuleFiles name []     = return []
getSpecificModuleFiles name (f:fs) = do
    source <- liftIO (readFile f)
    ast <- parse f
    if fromJust (S.astModuleName ast) == name then
        (f:) <$> getSpecificModuleFiles name fs
    else
        getSpecificModuleFiles name fs


lexFile :: BoM s m => FilePath -> m [L.Token]
lexFile file = do
    source <- liftIO (readFile file)
    case L.alexScanner file source of
        Left  errStr -> throwError (ErrorStr errStr)
        Right tokens -> return tokens


-- parse a file into an AST.
-- Throw an error on failure.
parse :: BoM s m => FilePath -> m S.AST
parse file = do
    source <- liftIO (readFile file)
    case P.parse file source of
        Left e  -> throwError e
        Right a -> return a


runMod :: BoM Modules m => Args -> Set.Set FilePath -> FilePath -> m CompileState
runMod args pathsVisited modPath = do
    debug "running"
    path <- checkAndNormalisePath modPath
    assert (not $ Set.member path pathsVisited) ("importing \"" ++ path ++ "\" forms a cycle")
    maybe (compilePath path) return =<< fmap (Map.lookup path) (gets modMap)
    where
        -- path will be in the form "dir1/dirn/modname"
        compilePath :: BoM Modules m => FilePath -> m CompileState
        compilePath path = do
            let modName = takeFileName path
            let modDirectory = takeDirectory path

            -- get files and create combined AST
            files <- getSpecificModuleFiles modName =<< getBoFilesInDirectory modDirectory
            assert (not $ null files) ("no files for: " ++ path)
            forM_ files $ debug . ("using file: " ++) 
            combinedAST <- combineASTs =<< mapM parse files

            when (printAst args) $ liftIO $ S.prettyAST combinedAST


            -- resolve imports into paths and check for collisions
            importPaths <- forM [fp | S.Import fp <- S.astImports combinedAST] $ \importPath ->
                checkAndNormalisePath $ joinPath [modDirectory, importPath]
            let importModNames = map takeFileName importPaths
            assert (length importModNames == length (Set.fromList importModNames)) $
                fail "import name collision"
            forM_ importPaths $ debug . ("importing: " ++)
            imports <- mapM (runMod args (Set.insert path pathsVisited)) importPaths


            -- load C imports
            let cFilePaths =  [ fp | S.ImportC fp <- S.astImports combinedAST]
            let cMacroStmts = [ Interop.importToCStmt imp  | imp@(S.ImportCMacro _ _) <- S.astImports combinedAST ]
            cTranslUnitEither <- liftIO $ withTempFile "." "cimports.h" $ \filePath handle -> do
                hClose handle

                writeFile filePath $ concat $
                    map (\p -> "#include \"" ++ p ++ "\"\n") cFilePaths ++
                    map (\p -> p ++ "\n") cMacroStmts

                --putStrLn =<< readFile filePath

                parseCFile (newGCC "gcc") Nothing [] filePath
            cTranslUnit <- case cTranslUnitEither of
                Left (ParseError x) -> fail (show x)
                Right cTranslUnit   -> return cTranslUnit
            (globs, errs) <- analyseCTranslUnit cTranslUnit
            ((), cExterns) <- withErrorPrefix "interop compile" $
                runBoMTExcept [] (Interop.genExternsFromGlobs globs)
            --((), cExterns) <- runBoMTExcept [] (Interop.genExterns cTranslUnit)
            --liftIO $ putStrLn $ render (pretty cTranslUnit)
            --liftIO $ mapM_ (putStrLn . show) cExterns
            cState <- Interop.compile cExterns
            when (printCSymbols args) $ liftIO $ do
                putStrLn $ "C Symbols imported by: " ++ modName 
                SymTab.prettySymTab (State.symTab cState)
            --liftIO $ putStrLn $ render $ pretty globs


            resSymTabMap <- gets resolveSymTabMap
            (resolvedAST, resolveState) <- withErrorPrefix "resolve: " $
                runBoMTExcept (R.initResolveState resSymTabMap modName) (R.resolve combinedAST)
            modify $ \s -> s { resolveSymTabMap = Map.insert path (R.symTab resolveState) (resolveSymTabMap s) }


            -- run type inference on ast
            annotatedAST <- fmap fst $ withErrorPrefix "annotate: " $
                runBoMTExcept 0 $ annotate resolvedAST
            (astInferred, symTab) <- withErrorPrefix "infer: " $ do
                stm <- gets symTabMap
                infer annotatedAST cExterns stm modName (verbose args)
            
            let ast = astInferred
            when (printFinalAst args) $ liftIO $ S.prettyAST ast
            --liftIO $ S.prettyAST ast
            --liftIO $ SymTab.prettySymTab symTab


            flat <- fmap fst $ withErrorPrefix "flatten: " $ runBoMTExcept initFlattenState (flattenAST ast)
            --assert (S.astModuleName flat == modName) "modName error"

            -- compile and run
            debug "compiling"
            (defs, state) <- withErrorPrefix "compile: " $ Compile.compile (cState : imports) flat
            when (printSymbols args) $ liftIO $ SymTab.prettySymTab (State.symTab state)

            debug "running"
            session <- gets session
            if compileObj args then do
                let modDirectory' = joinPath [modDirectory, "build"]
                let modName' = addExtension modName ".o"
                liftIO $ createDirectoryIfMissing True modDirectory'
                liftIO $ jitCompileToObject (printLLIR args) (joinPath [modDirectory', modName']) defs session
            else do
                liftIO $ jitAndRun defs session True (printLLIR args) 

            modify $ \s -> s { modMap = Map.insert path state (modMap s) }
            modify $ \s -> s { symTabMap = Map.insert modName symTab (symTabMap s) }
            return state

        debug str =
            if verbose args
            then liftIO $ putStrLn (modPath ++ " -> " ++ str)
            else return ()

