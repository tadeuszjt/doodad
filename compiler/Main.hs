module Main where

import System.Exit
import System.Environment
import Control.Monad

import Error
import Modules
import Args
import AST


main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = parseArgs initArgs args
    if astOnly parsedArgs then
        forM_ (modPaths parsedArgs) $ \path -> do
            ast <- parse parsedArgs path
            prettyAST ast

    else do
        forM_ (modPaths parsedArgs) $ \path -> do
            buildBinaryFromModule parsedArgs path
