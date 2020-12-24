{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules where

import Prelude hiding (fail)
import Control.Monad.Fail
import Control.Monad.State hiding (fail)
import Control.Monad.Except hiding (void, fail)

import qualified Data.Map as Map
import           Error
import qualified SymTab
import qualified AST as S
import qualified Lexer as L
import qualified Parser as P
import qualified Resolver as R
import qualified Flatten as F
import           CmpMonad
import           Value hiding (Module)
import           LLVM.AST hiding (Module, Name)

type ModuleName = String


data Module
    = ModuleAST
        { modASTs :: [S.AST]
        }
    | ModuleResolved
        { modAST :: S.AST
        , symMap :: Map.Map R.Symbol R.Name
        }
    | ModuleFlat
        { flatAST :: F.FlattenState
        }

initModule
    = ModuleAST
        { modASTs = []
        }


data ModulesState
    = ModulesState
        { modMap      :: Map.Map ModuleName Module         -- Map of all modules
        }

initModulesState
    = ModulesState
        { modMap     = Map.empty
        }


newtype ModulesT m a
    = ModulesT { getModules :: StateT ModulesState (ExceptT CmpError m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState ModulesState, MonadError CmpError)

class (MonadState ModulesState m, MonadFail m, MonadIO m) => MonadModules m

instance (MonadFail m, MonadIO m) => MonadModules (ModulesT m)

instance (Monad m, MonadFail m) => MonadFail (ModulesT m) where
    fail s = throwError $ CmpError (Nothing, s)


runModulesT :: Monad m => ModulesState -> ModulesT m a -> m (Either CmpError (a, ModulesState))
runModulesT moddulesState modulesT  =
    runExceptT $ runStateT (getModules modulesT) moddulesState



modModify :: MonadModules m => ModuleName -> (Maybe Module -> m Module) -> m ()
modModify modName f = do
    res <- fmap (Map.lookup modName) (gets modMap)
    mod' <- f res
    modify $ \s -> s { modMap = Map.insert modName mod' (modMap s) }


modAddAST :: MonadModules m => S.AST -> m ()
modAddAST ast = do
    let moduleName = maybe "main" id (S.astModuleName ast)
    modModify moduleName $ \res -> case res of
        Nothing  -> return $ initModule { modASTs = [ast] }
        Just mod -> return $ mod { modASTs = (modASTs mod) ++ [ast] }


modResolveSymbols :: MonadModules m => ModuleName -> m ()
modResolveSymbols modName = do
    ModuleAST asts <- fmap (Map.! modName) (gets modMap)
    let combinedAST = S.AST {
        S.astModuleName = Nothing,
        S.astStmts      = concat (map S.astStmts asts)
        }

    res <- R.resolveAST R.initResolverState combinedAST
    case res of
        Left err                    -> fail (show err)
        Right (ast', resolverState) -> modModify modName $ \_ -> do
            return $ ModuleResolved ast' (head $ R.symbolTable resolverState)
            
    return ()


modFlattenAST :: MonadModules m => ModuleName -> m ()
modFlattenAST modName = do
    ModuleAST asts <- fmap (Map.! modName) (gets modMap)
    let combinedAST = S.AST {
        S.astModuleName = Nothing,
        S.astStmts      = concat (map S.astStmts asts)
        }
    res <- F.flattenAST combinedAST
    case res of
        Left err    -> fail (show err)
        Right state -> modModify modName $ \_ -> return $ ModuleFlat state
    return ()


parse :: String -> Either CmpError S.AST
parse source =
    case L.alexScanner source of
        Left  errStr -> Left $ CmpError (Nothing, errStr)
        Right tokens -> case (P.parseTokens tokens) 0 of
            P.ParseFail pos -> Left $ CmpError (Just pos, "parse error")
            P.ParseOk ast   -> Right ast 



runFiles :: MonadModules m => [String] -> m ()
runFiles fs = do
    forM_ fs $ \f ->
        case parse f of
            Left err  -> liftIO (printError err f)
            Right ast -> modAddAST ast

    modMap <- gets modMap
    --forM_ (Map.keys modMap) $ \modName -> modResolveSymbols modName
    mapM_ modFlattenAST (Map.keys modMap)

    return ()


prettyModules :: ModulesState -> IO ()
prettyModules modules = do
    forM_ (Map.toList $ modMap modules) $ \(modName, mod) -> do
        case mod of
            ModuleAST asts -> do
                putStrLn "ModuleAST"
                mapM_ S.prettyAST asts
            ModuleResolved ast symbols -> do
                putStrLn ("ModuleResolved: " ++ modName)
                mapM_ (putStrLn . show) (Map.toList symbols)
                S.prettyAST ast
            ModuleFlat flatState -> do
                putStrLn ("ModuleFlat: " ++ modName)
                F.prettyFlatAST flatState
        putStrLn ""

