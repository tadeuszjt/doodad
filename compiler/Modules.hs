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

-- Modules are groups of .bo files with a module name header
-- lang/lexer.bo: lexer module


data Modules
    = Modules
        { modMap  :: Map.Map FilePath CompileState
        , session :: JIT.Session
        }


initModulesState session
    = Modules
        { modMap  = Map.empty
        , session = session
        }


checkAndNormalisePath :: BoM s m => FilePath -> m FilePath
checkAndNormalisePath path = do
    when (not $ isValid path) $ fail (show path ++ " invalid")
    processSplitPath (splitPath path)

    where
        processSplitPath :: BoM s m => [FilePath] -> m FilePath
        processSplitPath path = case path of
            [x]    -> checkValidModuleName x >> return x
            ("./":xs) -> processSplitPath xs
            (x:"../":xs) -> checkValidDirName x >> processSplitPath xs
            (x:xs) -> do
                checkValidDirName x
                xs' <- processSplitPath xs
                return $ joinPath [x, xs']
            xs -> fail ("Cannot process: " ++ joinPath xs)

        checkValidModuleName :: BoM s m => String -> m ()
        checkValidModuleName name = do
            when (null name) $ fail "Invalid module name"
            when (not $ isAlpha $ head name) $ fail $ "Module name: " ++ name ++ " must have alpha as first char"
            when (not $ all (== True) (map isAlphaNum name)) $ fail $ "Module name: " ++ name ++ " must be all alphanum"

        checkValidDirName :: BoM s m => String -> m ()
        checkValidDirName dirName = do
            let name = dropTrailingPathSeparator dirName
            when (null name) $ fail "Invalid directory name"
            when (not $ isAlpha $ head name) $ fail $ "Directory name: " ++ name ++ " must have alpha as first char"
            when (not $ all (== True) (map isAlphaNum name)) $ fail $ "Module name: " ++ name ++ " must be all alphanum"


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
        Left (ErrorStr str)         -> throwError (ErrorStr str)
        Left (ErrorSrc src pos str) -> throwError (ErrorFile file pos str)
        Left (ErrorFile "" pos str) -> throwError (ErrorFile file pos str)
        Right a                     -> return a


runMod :: BoM Modules m => Args -> Set.Set FilePath -> FilePath -> m CompileState
runMod args visited modPath = do
    debug "running"
    path <- checkAndNormalisePath modPath
    when (Set.member path visited) $
        fail ("importing \"" ++ path ++ "\" forms a cycle")

    resm <- Map.lookup path <$> gets modMap
    maybe (compile path) (return) resm
    where
        -- path will be in the form "dir1/dirn/modname"
        compile :: BoM Modules m => FilePath -> m CompileState
        compile path = do
            let name = takeFileName path
            let dir = takeDirectory path

            files <- getSpecificModuleFiles name =<< getBoFilesInDirectory dir
            when (null files) $ fail ("no files for: " ++ path)


            -- flatten asts
            asts <- forM (zip files [0..]) $ \(file, id) -> do
                debug ("using file: " ++ file)
                parse id file
            combinedAST <- combineASTs asts
            let importNames = map takeFileName (S.astImports combinedAST)
            when (length importNames /= length (Set.fromList importNames)) $
                fail "import name collision"

            imports <- fmap Map.fromList $ forM (S.astImports combinedAST) $ \importPath -> do
                resPath <- checkAndNormalisePath $ joinPath [dir, importPath]

                let importName = takeFileName resPath
                debug ("importing : " ++ importPath ++ " as " ++ importName)
                state <- runMod args (Set.insert path visited) resPath
                return (importName, state)

            flatRes <- runBoMT initFlattenState (flattenAST combinedAST)
            flat <- case flatRes of
                Left (ErrorFile "" pos str) -> throwError $ ErrorFile (files !! textFile pos) pos str
                Right ((), flatState)       -> return flatState

            -- compile and run
            debug "compiling"
            session <- gets session
            cmpRes <- runBoMT () $ compileFlatState imports flat name
            (defs, state) <- case cmpRes of
                Left (ErrorFile "" pos str) -> throwError $ ErrorFile (files !! textFile pos) pos str
                Left (ErrorStr str)         -> throwError $ ErrorStr str
                Right (res, _)              -> return res

            if compileObj args then do
                let dir' = joinPath [dir, "build"]
                let name' = addExtension name ".o"
                liftIO $ createDirectoryIfMissing True dir'
                liftIO $ jitCompileToObject (printLLIR args) (joinPath [dir', name']) defs session
            else do
                liftIO $ jitAndRun defs session True (printLLIR args) 

            modify $ \s -> s { modMap = Map.insert path state (modMap s) }
            return state

        debug str =
            if verbose args
            then liftIO $ putStrLn (show modPath ++ " -> " ++ str)
            else return ()

