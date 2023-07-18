module Main where

import System.Exit
import System.Environment
import System.IO
import System.Console.Haskeline
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set

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
            res <- runBoMT () $ parse parsedArgs path
            case res of
                Left err     -> printError err 
                Right (ast, _) -> prettyAST ast

    else do
        forM_ (modPaths parsedArgs) $ \path -> do
            res <- runBoMT undefined $ buildModule parsedArgs path
            case res of
                Left err -> printError err >> exitFailure
                Right _  -> return ()
