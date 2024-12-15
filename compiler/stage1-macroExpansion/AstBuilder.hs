{-# LANGUAGE FlexibleInstances #-}
module AstBuilder where

import Control.Monad.State
import Control.Monad.Identity

import AST
import Monad
import InstBuilder
import Error
import Type


data TopStmt
    = TopStmt Stmt
    | TopInst TextPos Generics Type [Param] Retty InstBuilderState

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
