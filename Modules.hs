{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules where

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
import qualified Flatten as F
import qualified Compile as C
import qualified CompileState as C
import           Monad
import           JIT


data Module
    = ModuleAST      [S.AST]
    | ModuleCompiled C.CompileState


data ModulesState
    = ModulesState
        { modMap  :: Map.Map S.ModuleName Module
        , session :: JIT.Session
        }

initModulesState session
    = ModulesState
        { modMap  = Map.empty
        , session = session
        }


modModify :: BoM ModulesState m => S.ModuleName -> (Maybe Module -> m Module) -> m ()
modModify modName f = do
    res <- fmap (Map.lookup modName) (gets modMap)
    mod' <- f res
    modify $ \s -> s { modMap = Map.insert modName mod' (modMap s) }


modAddAST :: BoM ModulesState m => S.AST -> m ()
modAddAST ast = do
    let name = maybe "main" id (S.astModuleName ast)
    modModify name $ \res -> case res of
        Nothing               -> return (ModuleAST [ast])
        Just (ModuleAST asts) -> return (ModuleAST (ast : asts))


modCompile :: BoM ModulesState m => S.ModuleName -> Bool -> m ()
modCompile modName verbose = 
    modCompileDep modName Set.empty
    where
        modCompileDep :: BoM ModulesState m => S.ModuleName -> Set.Set S.ModuleName -> m ()
        modCompileDep name visited = do
            when (Set.member name visited) $ fail ("circular dependency involving " ++ name)

            modModify name $ \res -> do
                when (isNothing res) $ fail ("module " ++ name ++ "doesn't exist")
                let ModuleAST asts = fromJust res

                combinedAST <- F.combineASTs asts
                let imports = S.astImports combinedAST
                res  <- runBoMT F.initFlattenState (F.flattenAST combinedAST)
                case res of
                    Left err              -> throwError err
                    Right ((), flatState) -> do
                        cmpImps <- forM (Set.toList $ imports) $ \imp -> do
                            modCompileDep imp (Set.insert name visited)
                            ModuleCompiled cmpState <- fmap (Map.! imp) (gets modMap)
                            return (imp, cmpState)

                        state <- C.compileFlatState (Map.fromList cmpImps) flatState
                        sess <- gets session
                        liftIO $ jitAndRun (C.definitions state) sess True verbose
                        return (ModuleCompiled state)


parse :: BoM s m => String -> m S.AST
parse source =
    case L.alexScanner source of
        Left  errStr -> fail errStr
        Right tokens -> case (P.parseTokens tokens) 0 of
            P.ParseFail pos -> throwError $ CmpError (Just pos, "parse error")
            P.ParseOk ast   -> return ast 


runFiles :: BoM ModulesState m => [String] -> Bool -> m ()
runFiles fs verbose = do
    forM_ fs $ \f -> modAddAST =<< parse f
    modCompile "main" verbose


prettyModules :: ModulesState -> IO ()
prettyModules modules = do
    forM_ (Map.toList $ modMap modules) $ \(modName, mod) -> do
        case mod of
            ModuleAST asts -> do
                putStrLn "ModuleAST"
                mapM_ (S.prettyAST "\t") asts
            ModuleCompiled state -> do
                putStrLn ("ModuleCompiled " ++ modName)
                C.prettyCompileState state
        putStrLn ""

