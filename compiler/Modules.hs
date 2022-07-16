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
import Collect as C
import Unify
import Annotate
import Apply
import Interop
import qualified Resolve as R

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
        , symTabMap :: Map.Map FilePath C.SymTab
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


runTypeInference :: BoM s m => Args -> AST -> [Extern] -> Map.Map FilePath C.SymTab -> m (AST, C.SymTab)
runTypeInference args annotatedAST externs imports = do
    cSymTab <- fmap (C.symTab . snd) $ runBoMTExcept (C.initCollectState Map.empty) (C.collectCExterns externs)
    assert (not $ Map.member "c" imports) "Modeule named c already imported"
    (ast, symTab, n) <- runRec annotatedAST (Map.insert "c" cSymTab $ Map.mapKeys takeFileName imports)
    when (verbose args) $ liftIO $ putStrLn ("Ran type inference with " ++ show n ++ " iterations")
    return (ast, symTab)
    where
        runRec :: BoM s m => AST -> Map.Map FilePath C.SymTab -> m (AST, C.SymTab, Int)
        runRec ast imports = do
            (_, state) <- runBoMTExcept (C.initCollectState imports) (C.collectAST ast)
            subs <- unify $ C.collected state
            defaults <- unifyDefault $ map (apply subs) (C.defaults state)
            let ast' = apply subs ast
            if ast == ast'
            then return (apply defaults ast', apply defaults (apply subs (C.symTab state)), 1)
            else do
                (ast'', symTab, n) <- runRec ast' imports
                return (ast'', symTab, n + 1)


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


            -- resolve imports into paths and check for collisions
            importPaths <- forM [fp | S.Import fp <- S.astImports combinedAST] $ \importPath ->
                checkAndNormalisePath $ joinPath [modDirectory, importPath]
            let importModNames = map takeFileName importPaths
            assert (length importModNames == length (Set.fromList importModNames)) $
                fail "import name collision"
            forM_ importPaths $ debug . ("importing: " ++)
            importMap <- fmap Map.fromList $ forM importPaths $ \importPath -> do
                state <- runMod args (Set.insert path pathsVisited) importPath
                return (takeFileName importPath, state)


            -- load C imports
            let cFilePaths = [fp | S.ImportC fp <- S.astImports combinedAST]
            cTranslUnitEither <- liftIO $ withTempFile "." "cimports.h" $ \filePath handle -> do
                hClose handle
                writeFile filePath $ concat $ map (\p -> "#include \"" ++ p ++ "\"\n") cFilePaths
                parseCFile (newGCC "gcc") Nothing [] filePath
            cTranslUnit <- case cTranslUnitEither of
                Left (ParseError x) -> fail (show x)
                Right cTranslUnit   -> return cTranslUnit
            --liftIO $ putStrLn $ render (pretty cTranslUnit)
            ((), cExterns) <- runBoMTExcept [] (Interop.genExterns cTranslUnit)
            --liftIO $ mapM_ (putStrLn . show) cExterns
            cState <- Interop.compile cExterns


            assert (not $ Map.member "c" importMap) "Already has a module named c"
            let importMapFull = Map.insert "c" cState importMap 
            

            resSymTabMap <- gets resolveSymTabMap
            (resolvedAST, resolveState) <- withErrorPrefix "resolve: " $
                runBoMTExcept (R.initResolveState resSymTabMap modName) (R.resolve combinedAST)
            liftIO $ S.prettyAST resolvedAST
            modify $ \s -> s { resolveSymTabMap = Map.insert path (R.symTab resolveState) (resolveSymTabMap s) }


            -- run type inference on ast
            annotatedAST <- fmap fst $ withErrorPrefix "annotate: " $
                runBoMTExcept 0 $ annotate combinedAST
            (ast, symTab) <- withErrorPrefix "infer: " $
                runTypeInference args annotatedAST cExterns =<< gets symTabMap
            liftIO $ S.prettyAST ast


            flat <- fmap fst $ runBoMTExcept initFlattenState (flattenAST ast)
            --assert (S.astModuleName flat == modName) "modName error"

            -- compile and run
            debug "compiling"
            (defs, state) <- withErrorPrefix "compile: " $ Compile.compile importMapFull flat

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
            modify $ \s -> s { symTabMap = Map.insert path symTab (symTabMap s) }
            return state

        debug str =
            if verbose args
            then liftIO $ putStrLn (modPath ++ " -> " ++ str)
            else return ()

