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

getFeatureArgFromFuncCall :: Symbol -> Type -> DoM ASTResolved Type
getFeatureArgFromFuncCall symbol callType = do
    liftIO $ putStrLn $ "getFeatureArgFromFuncCall: " ++ show callType
    ast <- get

    featureHeaderCandidates <- fmap concat $ forM (Map.toList $ featureDefsAll ast) $
        \(symbol, (arg, headers)) -> fmap catMaybes $ forM headers $ \header -> do
            symbolsMatch <- return $ symbolsCouldMatch symbol (funcSymbol header)
            let typesMatch = typesCouldMatch callType (typeof header)
            case symbolsMatch && typesMatch of
                True -> return $ Just (symbol, header)
                False -> return Nothing


    case featureHeaderCandidates of
        [x] -> do
            liftIO $ putStrLn $ "here: " ++ show x
        _ -> return ()


    return Void
            
