{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Flatten where
-- Walks an AST and resolves all symbols into unique names depending on scope.

import Control.Monad.State hiding (fail)
import Control.Monad.Fail
import qualified AST as S
import Monad
import Error



data FlattenState
    = FlattenState
        { typedefs  :: [S.Stmt]
        , variables :: [S.Stmt]
        , funcDefs  :: [S.Stmt]
        , externs   :: [S.Stmt]
        }

initFlattenState
    = FlattenState
        { typedefs  = []
        , variables = []
        , funcDefs  = []
        , externs   = []
        }



flattenAST :: (MonadIO m, MonadFail m) => S.AST -> m (Either CmpError FlattenState)
flattenAST ast = do
    res <- runBoMT initFlattenState f
    case res of
        Left err         -> return (Left err)
        Right (_, state) -> return (Right state)
    where
        f = mapM_ flattenStmt (S.astStmts ast)
        
        flattenStmt :: BoM FlattenState m => S.Stmt -> m ()
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
