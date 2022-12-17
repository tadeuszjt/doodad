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
import IRGen
import States


import Language.C
import Language.C.System.GCC
import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Pretty
import Language.C.Syntax.AST
import Text.PrettyPrint

-- Modules are groups of .doo files with a module name header
-- lang/lexer.doo: lexer module


data Modules
    = Modules
        { irGenModMap :: Map.Map FilePath IRGenState
        , session     :: JIT.Session
        }


initModulesState session
    = Modules
        { session     = session
        , irGenModMap = Map.empty
        }


getDoodadFilesInDirectory :: BoM s m => FilePath -> m [FilePath]
getDoodadFilesInDirectory dir = do
    list <- liftIO (listDirectory dir)
    return [ dir ++ "/" ++ f | f <- list, isSuffixOf ".doo" f ]


getSpecificModuleFiles :: BoM s m => String -> [FilePath] -> m [FilePath]
getSpecificModuleFiles name []     = return []
getSpecificModuleFiles name (f:fs) = do
    source <- liftIO (readFile f)
    ast <- parse f
    if (S.astModuleName ast) == name then
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


runMod :: BoM Modules m => Args -> Set.Set FilePath -> FilePath -> m ()
runMod args pathsVisited modPath = do
    debug "running"
    absolute <- liftIO $ canonicalizePath modPath
    debug $ "absolute path: " ++ show absolute
    isCompiled <- Map.member absolute <$> gets irGenModMap
    when (not isCompiled) $ compilePath absolute
    where
        -- path will be in the form "dir1/dirn/modname"
        compilePath :: BoM Modules m => FilePath -> m ()
        compilePath path = do
            debug "compilePath"
            let modName = takeFileName path
            let modDirectory = takeDirectory path

            -- get files and create combined AST
            files <- getSpecificModuleFiles modName =<< getDoodadFilesInDirectory modDirectory
            assert (not $ null files) ("no files for: " ++ path)
            forM_ files $ debug . ("using file: " ++) 
            asts <- mapM parse files
            when (printAst args) $ do
                forM_ asts $ \ast ->
                    liftIO $ S.prettyAST ast


            importPaths <- forM [fp | S.Import fp <- concat $ map S.astImports asts] $ \importPath ->
                liftIO $ canonicalizePath $ joinPath [modDirectory, importPath]
            let importModNames = map takeFileName importPaths
            assert (length importModNames == length (Set.fromList importModNames)) $
                fail "import name collision"
            forM_ importPaths $ debug . ("importing: " ++)
            mapM_ (runMod args (Set.insert path pathsVisited)) importPaths

            debug "loading c imports"
            let cFilePaths =  [ fp | S.ImportC fp <- concat $ map S.astImports asts]
            let cMacroStmts = [ Interop.importToCStmt imp  | imp@(S.ImportCMacro _ _) <- concat $ map S.astImports asts ]
            cTranslUnitEither <- liftIO $ withTempFile "." "cimports.h" $ \filePath handle -> do
                hClose handle
                writeFile filePath $ concat $
                    map (\p -> "#include \"" ++ p ++ "\"\n") cFilePaths ++
                    map (\p -> p ++ "\n") cMacroStmts
                parseCFile (newGCC "gcc") Nothing [] filePath
            cTranslUnit <- case cTranslUnitEither of
                Left (ParseError x) -> fail (show x)
                Right cTranslUnit   -> return cTranslUnit
            (globs, errs) <- analyseCTranslUnit cTranslUnit
            ((), cExterns) <- withErrorPrefix "interop compile" $
                runBoMTExcept [] (Interop.genExternsFromGlobs globs)
            ((), cIrGenState) <- runBoMTExcept (initIRGenState "c") (mapM_ irGenExtern cExterns)


            debug "loading irGenImports"
            irGenImports <- forM importPaths $ \path -> do
                resm <- Map.lookup path <$> gets irGenModMap
                assert (isJust resm) $ show path ++ " not in irGenModMap"
                return $ fromJust resm
            (resolvedAST, _) <- R.resolveAsts asts (cIrGenState : irGenImports)
            Flatten.checkTypeDefs (typeDefs resolvedAST)
            when (printAstResolved args) $ liftIO $ prettyResolvedAst resolvedAST

            debug "annotating ast"
            annotatedAST <- fmap fst $ withErrorPrefix "annotate: " $
                runBoMTExcept 0 $ annotate resolvedAST
            astInferred <- withErrorPrefix "infer: " $ infer annotatedAST (verbose args)
            
            (_, irGenState) <- withErrorPrefix "irgen: " $
                runBoMTExcept (initIRGenState modName) (IRGen.compile astInferred)
            modify $ \s -> s { irGenModMap = Map.insert path (irGenState) (irGenModMap s) }
            debug "added to irGenModMap"
            when (printIR args) $ liftIO $ prettyIrGenState irGenState

            -- compile and run
            debug "compiling"
            session <- gets session
            (defs, state) <- withErrorPrefix "compile: " $ Compile.compile irGenState session
            when (printSymbols args) $ liftIO $ SymTab.prettySymTab (State.symTab state)

            debug "running"
            if compileObj args then do
                let modDirectory' = joinPath [modDirectory, "build"]
                let modName' = addExtension modName ".o"
                liftIO $ createDirectoryIfMissing True modDirectory'
                liftIO $ jitCompileToObject (printLLIR args) (joinPath [modDirectory', modName']) defs session
            else do
                liftIO $ jitAndRun defs session True (printLLIR args) 

        debug str =
            if verbose args
            then liftIO $ putStrLn (takeFileName modPath ++ " -> " ++ str)
            else return ()

