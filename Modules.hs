{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules where

import Prelude hiding (fail)
import Control.Monad.Fail
import Control.Monad.State hiding (fail)
import Control.Monad.Except hiding (void, fail)

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Maybe
import           Error
import qualified SymTab
import qualified AST as S
import qualified Lexer as L
import qualified Parser as P
import qualified Resolver as R
import qualified Flatten as F
import qualified Compile as C
import           CmpMonad
import           Value hiding (Module)
import           LLVM.AST hiding (Module, Name)
import Monad


data Module
    = ModuleAST [S.AST]
    | ModuleFlat F.FlattenState
    | ModuleCompiled C.CompileState


data ModulesState
    = ModulesState
        { modMap :: Map.Map S.ModuleName Module         -- Map of all modules
        }

initModulesState
    = ModulesState
        { modMap = Map.empty
        }


modModify :: BoM ModulesState m => S.ModuleName -> (Maybe Module -> m Module) -> m ()
modModify modName f = do
    res <- fmap (Map.lookup modName) (gets modMap)
    mod' <- f res
    modify $ \s -> s { modMap = Map.insert modName mod' (modMap s) }


modAddAST :: BoM ModulesState m => S.AST -> m ()
modAddAST ast = do
    let name    = maybe "main" id (S.astModuleName ast)
    modModify name $ \res -> case res of
        Nothing               -> return (ModuleAST [ast])
        Just (ModuleAST asts) -> return (ModuleAST (ast : asts))


modFlattenAST :: BoM ModulesState m => S.ModuleName -> m ()
modFlattenAST modName = do
    ModuleAST asts <- fmap (Map.! modName) (gets modMap)

    let imports = foldr1 Set.union (map S.astImports asts)
    when (Set.member modName imports) $ fail ("cannot import this module: " ++ modName)

    importFlatMap <- fmap Map.fromList $ forM (Set.toList imports) $ \imp -> do
        res <- fmap (Map.lookup imp) (gets modMap)
        when (isNothing res) $ fail (imp ++ " isn't in modMap")
        ModuleFlat flatState <- fmap (Map.! imp) (gets modMap)
        return (imp, flatState)

    let combinedAST = S.AST {
        S.astModuleName = Just modName,
        S.astImports    = imports,
        S.astStmts      = concat (map S.astStmts asts)
        }

    res <- F.flattenAST importFlatMap combinedAST
    case res of
        Left err    -> fail (show err)
        Right state -> modModify modName $ \_ -> return $ ModuleFlat state


modCompile :: BoM ModulesState m => S.ModuleName -> m ()
modCompile modName = 
    modCompileDep modName Set.empty
    where
        modCompileDep :: BoM ModulesState m => S.ModuleName -> Set.Set S.ModuleName -> m ()
        modCompileDep depName modsVisited = do
            when (Set.member depName modsVisited) $
                fail ("circular dependency involving " ++ depName)
            modModify depName $ \res -> case res of
                Nothing                 -> fail (depName ++ " doesn't exist")
                Just (ModuleFlat flatState) -> do
                    imports <- forM (Map.keys $ F.importFlat flatState) $ \imp -> do
                        modCompileDep imp (Set.insert depName modsVisited)
                        mod@(ModuleCompiled state) <- fmap (Map.! imp) (gets modMap)
                        return (imp, state)

                    res <- C.compileFlatState (Map.fromList imports) flatState
                    case res of
                        Left err    -> throwError err
                        Right state -> return (ModuleCompiled state)


parse :: String -> Either CmpError S.AST
parse source =
    case L.alexScanner source of
        Left  errStr -> Left $ CmpError (Nothing, errStr)
        Right tokens -> case (P.parseTokens tokens) 0 of
            P.ParseFail pos -> Left $ CmpError (Just pos, "parse error")
            P.ParseOk ast   -> Right ast 


runFiles :: BoM ModulesState m => [String] -> m ()
runFiles fs = do
    forM_ fs $ \f ->
        case parse f of
            Left err  -> liftIO (printError err f)
            Right ast -> modAddAST ast

    modMap <- gets modMap
    mapM_ modFlattenAST (Map.keys modMap)
    --modCompile "main"


prettyModules :: ModulesState -> IO ()
prettyModules modules = do
    forM_ (Map.toList $ modMap modules) $ \(modName, mod) -> do
        case mod of
            ModuleAST asts -> do
                putStrLn "ModuleAST"
                mapM_ (S.prettyAST "\t") asts
            ModuleFlat flatState -> do
                putStrLn ("ModuleFlat: " ++ modName)
                putStrLn ("Imports:")
                mapM_ (putStrLn . show) (Map.keys $ F.importFlat flatState)
                F.prettyFlatAST flatState
            ModuleCompiled state -> do
                putStrLn ("ModuleCompiled " ++ modName)
                forM_ (C.defs state) $ \d ->
                    putStrLn $ take 100 (show d)
        putStrLn ""

