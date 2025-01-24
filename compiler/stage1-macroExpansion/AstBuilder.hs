module AstBuilder where

import Control.Monad.State
import Control.Monad.Identity

import AST
import InstBuilder
import Error
import Type


data TopStmt
    = TopStmt Stmt
    | TopInst TextPos Generics Type [Param] Bool InstBuilderState
    | TopField TextPos Generics Type Int


data AstBuilderState = AstBuilderState
    { topStmts :: [TopStmt]
    , abModuleName :: String
    , abImports    :: [Import]
    }


class (Monad m) => MonadAstBuilder m where
    liftAstBuilderState :: State AstBuilderState a -> m a


initAstBuilderState name imports = AstBuilderState
    { topStmts     = []
    , abModuleName = name
    , abImports    = imports
    }


addTopStmt :: MonadAstBuilder m => TopStmt -> m ()
addTopStmt stmt =
    liftAstBuilderState $ modify $ \s -> s { topStmts = (topStmts s) ++ [stmt] }
