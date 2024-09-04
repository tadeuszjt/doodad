{-# LANGUAGE FlexibleInstances #-}
module AstBuilder where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity

import AST
import Monad
import InstBuilder
import Error
import Type


data TopStmt
    = TopStmt Stmt
    | TopInst TextPos Generics Type [Param] Bool InstBuilderState

data AstBuilderState = AstBuilderState
    { topStmts :: [TopStmt]
    , abModuleName :: String
    , abImports    :: [Import]
    }

class (Monad m, MonadFail m) => MonadAstBuilder m where
    liftAstBuilderState :: State AstBuilderState a -> m a


instance MonadAstBuilder (DoM AstBuilderState) where
    liftAstBuilderState (StateT s) = DoM $ StateT (pure . runIdentity . s)


initAstBuilderState name imports = AstBuilderState
    { topStmts = []
    , abModuleName = name
    , abImports    = imports
    }


addTopStmt :: MonadAstBuilder m => TopStmt -> m ()
addTopStmt stmt =
    liftAstBuilderState $ modify $ \s -> s { topStmts = (topStmts s) ++ [stmt] }


unbuildStmt :: AstBuilderState -> TopStmt -> DoM () Stmt
unbuildStmt state statement = case statement of
    TopStmt s -> return s
    TopInst pos generics typ args retty instState -> do
        Instance pos generics typ args retty <$> unbuildInst instState 0
