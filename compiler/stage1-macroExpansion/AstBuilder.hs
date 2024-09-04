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


unbuildInst :: InstBuilderState -> ID -> DoM () Stmt
unbuildInst instState id = do
    let Just stmt = Map.lookup id (statements instState)
    case stmt of
        Block stmts -> fmap Block $ forM stmts $ \(Stmt id) -> unbuildInst instState id
        Return pos mexpr -> return stmt
        ExprStmt expr    -> return stmt
        EmbedC _ _ _     -> return stmt
        Let _ _ _ _      -> return stmt
        Assign _ _ _     -> return stmt

        If pos expr (Stmt trueId) mfalse -> do
            true' <- unbuildInst instState trueId
            false' <- case mfalse of
                Nothing -> return Nothing
                Just (Stmt id) -> fmap Just $ unbuildInst instState id

            return $ If pos expr true' false'

        While pos expr (Stmt id) -> While pos expr <$> unbuildInst instState id


        x -> error (show x)
