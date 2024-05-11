{-# LANGUAGE FlexibleInstances #-}
module AstBuilder where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity

import AST
import Monad


type ID = Int

globalId = 0

data AstBuilderState = AstBuilderState
    { statements :: Map.Map ID Stmt
    , idSupply   :: ID
    , curId      :: ID
    }

class (Monad m, MonadFail m) => MonadAstBuilder m where
    liftAstBuilderState :: State AstBuilderState a -> m a


instance MonadAstBuilder (DoM AstBuilderState) where
    liftAstBuilderState (StateT s) = DoM $ StateT (pure . runIdentity . s)


initAstBuilderState = AstBuilderState
    { statements = Map.singleton globalId (Block [])
    , idSupply   = globalId + 1
    , curId      = globalId
    }


generateId :: MonadAstBuilder m => m ID
generateId = liftAstBuilderState $ do
    idSupply <- gets idSupply
    modify $ \s -> s { idSupply = idSupply + 1 }
    return idSupply


getCurId :: MonadAstBuilder m => m ID
getCurId = do
    liftAstBuilderState (gets curId)


appendId :: MonadAstBuilder m => ID -> m ()
appendId id = do
    curId <- liftAstBuilderState (gets curId)
    True <- liftAstBuilderState $ gets (Map.member curId . statements)
    stmt <- liftAstBuilderState $ gets $ (Map.! curId) . statements
    stmt' <- case stmt of
        Block ids -> return $ Block (ids ++ [Stmt id])
        x -> error (show x)

    liftAstBuilderState $ modify $ \s -> s { statements = Map.insert curId stmt' (statements s) }


newStmt :: MonadAstBuilder m => Stmt -> m ID
newStmt stmt = do
    id <- generateId
    liftAstBuilderState $ modify $ \s -> s { statements = Map.insert id stmt (statements s) }
    return id


appendStmt :: MonadAstBuilder m => Stmt -> m ID
appendStmt stmt = do
    id <- generateId
    appendId id
    liftAstBuilderState $ modify $ \s -> s { statements = Map.insert id stmt (statements s) }
    return id


withCurId :: MonadAstBuilder m => ID -> (m a) -> m a
withCurId id f = do
    prevId <- liftAstBuilderState (gets curId)
    liftAstBuilderState $ modify $ \s -> s { curId = id }
    a <- f
    liftAstBuilderState $ modify $ \s -> s { curId = prevId }
    return a


