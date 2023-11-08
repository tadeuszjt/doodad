module Main where

import System.Exit
import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Except

import Error
import Modules
import Monad
import Args
import AST
import qualified Parser as P
import qualified SymTab
import Resolve
import Lexer


main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = parseArgs initArgs args
    if astOnly parsedArgs then
        forM_ (modPaths parsedArgs) $ \path -> do
            res <- runDoM () $ parse parsedArgs path
            case res of
                Left err     -> printError err 
                Right (ast, _) -> prettyAST ast

    else do
        forM_ (modPaths parsedArgs) $ \path -> do
            res <- runDoM () $ buildBinaryFromModule parsedArgs path
            case res of
                Left err -> printError err >> exitFailure
                Right _  -> return ()
