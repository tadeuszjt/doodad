module FeatureFinder where

import Control.Monad.IO.Class
import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

import Monad
import AST
import ASTResolved
import Type
import Symbol



-- eg: At::at(I64.Table, G) V ==>   Table{I64} -> use feature instance
-- eg: At::at(T.Table, G) V   ==>   Table{T}   -> use feature instance
-- eg: At::at(T, G) V         ==>   T          -> use feature header

getFeatureArgFromFuncCall :: CallHeader -> DoM ASTResolved Type
getFeatureArgFromFuncCall call = do
    liftIO $ putStrLn $ "getFeatureArgFromFuncCall: " ++ show call
    ast <- get

    featureHeaderCandidates <- fmap concat $ forM (Map.toList $ featureDefsAll ast) $
        \(symbol, (arg, headers)) -> fmap catMaybes $ forM headers $ \header -> do
            couldMatch <- callCouldMatchFunc call header
            case couldMatch of
                True -> return (Just header)
                False -> return Nothing

    forM_ featureHeaderCandidates $ \header -> do
        liftIO $ putStrLn $ "\t" ++ prettySymbol (funcSymbol header)


    return Void
            
