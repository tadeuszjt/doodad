{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Flatten where
-- Walks an AST and resolves all symbols into unique names depending on scope.

import Prelude hiding (fail)
import Control.Monad.State hiding (fail)
import Control.Monad.Fail
import Control.Monad.Except hiding (void, fail)
import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified SymTab
import qualified Type as T
import Error



data FlattenState
    = FlattenState
        { typedefs  :: [S.Stmt]
        , variables :: [S.Stmt]
        , funcDefs  :: [S.Stmt]
        , externs   :: [S.Stmt]
        }
    deriving Show


initFlattenState
    = FlattenState
        { typedefs  = []
        , variables = []
        , funcDefs  = []
        , externs   = []
        }


newtype FlattenT m a
    = FlattenT { getFlatten :: StateT FlattenState (ExceptT CmpError m) a }
    deriving (Functor, Applicative, Monad, MonadState FlattenState, MonadError CmpError)


runFlattenT :: Monad m => FlattenState -> FlattenT m a -> m (Either CmpError (a, FlattenState))
runFlattenT flattenState flattenT =
    runExceptT $ runStateT (getFlatten flattenT) flattenState


class (MonadState FlattenState m, MonadFail m) => MonadFlatten m

instance (MonadFail m) => MonadFlatten (FlattenT m)

instance (Monad m, MonadFail m) => MonadFail (FlattenT m) where
    fail s = throwError $ CmpError (Nothing, s)



flattenAST :: MonadFail m => S.AST -> m (Either CmpError FlattenState)
flattenAST ast = do
    res <- runFlattenT initFlattenState f
    case res of
        Left err         -> return (Left err)
        Right (_, state) -> return (Right state)
    where
        f = mapM_ flattenStmt (S.astStmts  ast)
        
        flattenStmt :: MonadFlatten m => S.Stmt -> m ()
        flattenStmt stmt = case stmt of
            S.Typedef _ _ _  -> modify $ \s -> s { typedefs = (typedefs s) ++ [stmt] }
            S.Assign _ _ _   -> modify $ \s -> s { variables = (variables s) ++ [stmt] }
            S.Func _ _ _ _ _ -> modify $ \s -> s { funcDefs = (funcDefs s) ++ [stmt] }
            S.Extern _ _ _ _ -> modify $ \s -> s { externs = (externs s) ++ [stmt] }
            _ -> return ()



prettyFlatAST :: FlattenState -> IO ()
prettyFlatAST flatAST = do
    putStrLn "Typedefs:"
    forM_ (typedefs flatAST) $ \typedef -> putStrLn ("\t" ++ show typedef)
    putStrLn "Variables:"
    forM_ (variables flatAST) $ \var -> putStrLn ("\t" ++ show var)
    putStrLn "Externs:"
    forM_ (externs flatAST) $ \(S.Extern pos name params retty) ->
        putStrLn $ "\t" ++ name ++ " " ++ (show params) ++ " " ++ show retty
    putStrLn "Functions:"
    forM_ (funcDefs flatAST) $ \(S.Func pos name params retty _) ->
        putStrLn $ "\t" ++ name ++ " " ++ (show params) ++ " " ++ show retty
