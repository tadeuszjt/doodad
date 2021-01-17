{-# LANGUAGE FlexibleContexts #-}
module Modules2 where

import System.Environment
import Control.Monad.IO.Class
import Control.Monad.Except hiding (void, fail)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified Parser as P
import qualified Lexer as L
import Monad
import Error

data Module
    = Module
        { filepaths :: Set.Set String
        , result    :: Maybe ()
        }


data Modules
    = Modules
        { modMap :: Map.Map String Module
        }


runFile :: BoM Modules m => String -> m ()
runFile filename = do
    source <- liftIO (readFile filename)
    ast <- parse filename source
    return ()


parse :: BoM s m => String -> String -> m S.AST
parse filename source =
    case L.alexScanner source of
        Left  errStr -> fail errStr
        Right tokens -> case (P.parseTokens tokens) 0 of
            P.ParseFail pos -> throwError (ErrorFile filename pos "parse error")
            P.ParseOk ast   -> return ast 
