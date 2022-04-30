module Main where

import System.Environment
import System.IO
import System.Console.Haskeline
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set

import JIT
import Error
import Modules
import Monad
import Args
import AST
import Infer
import qualified Parser as P
import qualified Lexer as L
import qualified SymTab

main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = parseArgs initArgs args
    if args == [] then do
        putStrLn "==== Bolang REPL ===="
--        withSession True $ \session -> do
--            runInputT defaultSettings (repl session $ initInferState Map.empty)
    else if lexOnly parsedArgs then do
        withSession (optimise parsedArgs) $ \session -> do
            forM_ (modPaths parsedArgs) $ \path -> do
                res <- runBoMT (initModulesState session) (lexFile 0 path)
                case res of
                    Left err     -> printError err 
                    Right (r, _) -> mapM_ (putStrLn . show) r
    else if astOnly parsedArgs then
        forM_ (modPaths parsedArgs) $ \path -> do
            res <- runBoMT () (parse 0 path)
            case res of
                Left err     -> printError err 
                Right (ast, _) -> prettyAST ast

    else if inferOnly parsedArgs then do
        forM_ (modPaths parsedArgs) $ \path -> do
            res <- runBoMT initRunInferState (runModInfer parsedArgs path Set.empty)
            case res of
                Left err     -> printError err 
                Right ((ast, state), y) -> return()
    else do
        withSession (optimise parsedArgs) $ \session -> do
            forM_ (modPaths parsedArgs) $ \path -> do
                res <- runBoMT (initModulesState session) $ runMod parsedArgs Set.empty path
                case res of
                    Left err -> printError err 
                    Right _  -> return ()


--repl :: Session -> InferState -> InputT IO ()
--repl session state = do
--    minput <- handleInterrupt (return $ Just "Ctrl-C exit") $ getInputLine "8===D "
--    case minput of
--        Nothing   -> return ()
--        Just "q"  -> return ()
--        Just line -> do
--            res <- runExceptT $ P.parse 0 line
--            case res of
--                Left e -> liftIO (printError e)
--                Right (AST _ _ [stmt]) -> do
--                    res <- liftIO $ runBoMT state (do { s' <- infStmt stmt; infResolve; return s' })
--                    case res of
--                        Left e -> liftIO $ printError e
--                        Right (stmt', state') -> do
--                            outputStrLn $ show stmt'
--                            liftIO $ prettyInferState state'
--                            repl session state'
