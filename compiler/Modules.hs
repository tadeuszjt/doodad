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

import Lexer
import qualified AST as S
import qualified Parser as P
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
import Resolve2
import CBuilder as C
import CPretty as C
import CGenerate as C


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
        { moduleMap   :: Map.Map FilePath ResolvedAst
        , session     :: JIT.Session
        }


initModulesState session
    = Modules
        { session     = session
        , moduleMap   = Map.empty
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
    newTokens <- lexFile file
    when (printTokens args) $ do
        liftIO $ mapM_ (putStrLn . show) newTokens
    P.parse newTokens


runMod :: BoM Modules m => Args -> Set.Set FilePath -> FilePath -> m ()
runMod args pathsVisited modPath = do
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
            ((), cResolvedAst) <- runBoMTExcept (initResolvedAst "c") (mapM_ astGenExtern cExterns)


            debug "loading irGenImports"
            astImports <- forM importPaths $ \path -> do
                resm <- Map.lookup path <$> gets moduleMap
                assert (isJust resm) $ show path ++ " not in module map"
                return $ fromJust resm
            (resolvedAST, _) <- R.resolveAsts asts (cResolvedAst : astImports) 
            Flatten.checkTypeDefs (typeDefs resolvedAST)
            when (printAstResolved args) $ liftIO $ prettyResolvedAst resolvedAST

            debug "annotating ast"
            annotatedAST <- fmap fst $ withErrorPrefix "annotate: " $
                runBoMTExcept 0 $ annotate resolvedAST
            astInferred <- withErrorPrefix "infer: " $ infer annotatedAST (verbose args)
            when (printAstFinal args) $ liftIO $ prettyResolvedAst astInferred


            debug "resolve2"
            ((), resolved2) <- withErrorPrefix "resolve2: " $ runBoMTExcept astInferred Resolve2.compile
            modify $ \s -> s { moduleMap = Map.insert path (resolved2) (moduleMap s) }




            -- test CBuilder
            buildDir <- liftIO $ canonicalizePath $ "build"
            liftIO $ createDirectoryIfMissing True buildDir
            let cFilePath = joinPath [buildDir, modName ++ ".c"]
            --cHandle <- liftIO $ openFile cFilePath WriteMode
            ((), cBuilderState) <- runBoMTExcept (C.initBuilderState modName) (generate resolved2)
            _ <- runBoMTExcept (initCPrettyState stdout cBuilderState) cPretty
            --liftIO $ hClose cHandle

            
            (_, irGenState) <- withErrorPrefix "irgen: " $
                runBoMTExcept (initIRGenState modName) (IRGen.compile resolved2)
            when (printIR args) $ liftIO $ prettyIrGenState irGenState

            -- compile and run
            debug "compiling"
            session <- gets session
            (defs, state) <- withErrorPrefix "compile: " $ Compile.compile irGenState session
            --when (printSymbols args) $ liftIO $ SymTab.prettySymTab (State.symTab state)

            debug "running"
            if compileObj args then do
                let modDirectory' = joinPath [modDirectory, "build"]
                let modName' = addExtension modName ".o"
                buildDir <- liftIO $ canonicalizePath $ "build"
                let fileDir = joinPath [buildDir, modName']
                liftIO $ putStrLn $ show fileDir

                liftIO $ createDirectoryIfMissing True buildDir
                liftIO $ jitCompileToObject (printLLIR args) fileDir defs session
            else do
                liftIO $ jitAndRun defs session True (printLLIR args) 

        debug str =
            if verbose args
            then liftIO $ putStrLn (takeFileName modPath ++ " -> " ++ str)
            else return ()

