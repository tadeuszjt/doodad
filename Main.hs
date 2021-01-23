module Main where

import System.Environment
import System.IO
import Control.Monad
import Data.List.Split
import qualified Data.Map as Map

import JIT
import Error
import Modules
import Monad
import Args


main :: IO ()
main = do
    args <- fmap (parseArgs initArgs) getArgs
    withSession (optimise args) $ \session -> do
        forM_ (modPaths args) $ \path -> do
            res <- runBoMT (initModulesState session) $ runMod args (splitOn "/" path)
            case res of
                Left err -> printError err Map.empty
                Right _  -> return ()
