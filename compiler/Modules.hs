{-# LANGUAGE FlexibleContexts #-}
module Modules where

import System.FilePath
import System.IO
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

-- Modules are groups of .bo files with a module name header
-- lang/lexer.bo: lexer module


data Modules
    = Modules
        { modMap  :: Map.Map FilePath CompileState
        , symTabMap :: Map.Map FilePath C.SymTab
        , session :: JIT.Session
        }


initModulesState session
    = Modules
        { modMap  = Map.empty
        , session = session
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
    ast <- parse 0 f
    if fromJust (S.astModuleName ast) == name then
        (f:) <$> getSpecificModuleFiles name fs
    else
        getSpecificModuleFiles name fs


lexFile :: BoM s m => Int -> FilePath -> m [L.Token]
lexFile id file = do
    source <- liftIO (readFile file)
    case L.alexScanner id source of
        Left  errStr -> throwError (ErrorStr errStr)
        Right tokens -> return tokens


-- parse a file into an AST.
-- Throw an error on failure.
parse :: BoM s m => Int -> FilePath -> m S.AST
parse id file = do
    source <- liftIO (readFile file)
    case P.parse id source of
        Left (ErrorStr str)         -> throwError (ErrorStr $ file ++ ": " ++ str)
        Left (ErrorPos pos str)     -> throwError (ErrorFile file pos str)
        Right a                     -> return a


runTypeInference :: BoM s m => Args -> AST -> Map.Map FilePath C.SymTab -> m (AST, C.SymTab)
runTypeInference args annotatedAST imports = do
    (ast, symTab, n) <- runRec annotatedAST imports
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
            then return (apply defaults ast', C.symTab state, 1)
            else do
                (ast'', symTab, n) <- runRec ast' imports
                return (ast'', symTab, n + 1)


runMod :: BoM Modules m => Args -> Set.Set FilePath -> FilePath -> m CompileState
runMod args pathsVisited modPath = do
    debug "running"
    path <- checkAndNormalisePath modPath
    assert (not $ Set.member path pathsVisited) ("importing \"" ++ path ++ "\" forms a cycle")
    resm <- Map.lookup path <$> gets modMap
    maybe (compile path) (return) resm
    where
        -- path will be in the form "dir1/dirn/modname"
        compile :: BoM Modules m => FilePath -> m CompileState
        compile path = do
            let modName = takeFileName path
            let modDirectory = takeDirectory path

            -- get files and create combined AST
            files <- getSpecificModuleFiles modName =<< getBoFilesInDirectory modDirectory
            assert (not $ null files) ("no files for: " ++ path)
            forM_ files $ debug . ("using file: " ++) 
            combinedAST <- combineASTs =<< zipWithM parse [0..] files


            -- resolve imports into paths and check for collisions
            importPaths <- forM (S.astImports combinedAST) $ \importPath ->
                checkAndNormalisePath $ joinPath [modDirectory, importPath]
            let importModNames = map takeFileName importPaths
            assert (length importModNames == length (Set.fromList importModNames)) $
                fail "import name collision"
            forM_ importPaths $ debug . ("importing: " ++)
            importMap <- fmap Map.fromList $ forM importPaths $ \importPath -> do
                state <- runMod args (Set.insert path pathsVisited) importPath
                return (takeFileName importPath, state)


            -- run type inference on ast
            annotatedAST <- fmap fst $ withFiles files $ runBoMTExcept 0 (annotateAST combinedAST)
            (ast, symTab) <- withFiles files $ runTypeInference args annotatedAST =<< gets symTabMap


            flat <- fmap snd $ withFiles files $ runBoMTExcept initFlattenState (flattenAST ast)

            -- compile and run
            debug "compiling"
            (defs, state) <- fmap fst $ withFiles files $ runBoMTExcept () $ compileFlatState importMap flat modName

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

