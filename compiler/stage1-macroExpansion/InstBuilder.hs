{-# LANGUAGE FlexibleInstances #-}
module InstBuilder where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity

import AST
import Monad
import Type


type ID = Int

globalId = 0

data InstBuilderState = InstBuilderState
    { statements    :: Map.Map ID Stmt
    , expressions   :: Map.Map ID Expr
    , patterns      :: Map.Map ID Pattern
    , types         :: Map.Map ID Type
    , idSupply      :: ID
    , curId         :: ID
    }

class (Monad m, MonadFail m) => MonadInstBuilder m where
    liftInstBuilderState :: State InstBuilderState a -> m a


instance MonadInstBuilder (DoM InstBuilderState) where
    liftInstBuilderState (StateT s) = DoM $ StateT (pure . runIdentity . s)


initInstBuilderState = InstBuilderState
    { statements = Map.singleton globalId (Block [])
    , expressions = Map.empty
    , patterns    = Map.empty
    , types      = Map.empty
    , idSupply   = globalId + 1
    , curId      = globalId
    }



generateId :: MonadInstBuilder m => m ID
generateId = liftInstBuilderState $ do
    idSupply <- gets idSupply
    modify $ \s -> s { idSupply = idSupply + 1 }
    return idSupply


getCurId :: MonadInstBuilder m => m ID
getCurId = do
    liftInstBuilderState (gets curId)


appendId :: MonadInstBuilder m => ID -> m ()
appendId id = do
    curId <- liftInstBuilderState (gets curId)
    True <- liftInstBuilderState $ gets (Map.member curId . statements)
    stmt <- liftInstBuilderState $ gets $ (Map.! curId) . statements
    stmt' <- case stmt of
        Block ids -> return $ Block (ids ++ [Stmt id])
        x -> error (show x)

    liftInstBuilderState $ modify $ \s -> s { statements = Map.insert curId stmt' (statements s) }


newStmt :: MonadInstBuilder m => Stmt -> m ID
newStmt stmt = do
    id <- generateId
    liftInstBuilderState $ modify $ \s -> s { statements = Map.insert id stmt (statements s) }
    return id


newType :: MonadInstBuilder m => ID -> Type -> m ID
newType id typ = do
    --resm <- liftInstBuilderState $ gets $ Map.lookup id . types
    --unless (isNothing resm) (fail $ "id already typed: " ++ show id)
    liftInstBuilderState $ modify $ \s -> s { types = Map.insert id typ (types s) }
    return id


newExpr :: MonadInstBuilder m => Expr -> m ID
newExpr expr = do
    id <- generateId
    liftInstBuilderState $ modify $ \s -> s { expressions = Map.insert id expr (expressions s) }
    return id

newPattern :: MonadInstBuilder m => Pattern -> m ID
newPattern pattern = do
    id <- generateId
    liftInstBuilderState $ modify $ \s -> s { patterns = Map.insert id pattern (patterns s) }
    return id

appendStmt :: MonadInstBuilder m => Stmt -> m ID
appendStmt stmt = do
    id <- generateId
    appendId id
    liftInstBuilderState $ modify $ \s -> s { statements = Map.insert id stmt (statements s) }
    return id


withCurId :: MonadInstBuilder m => ID -> (m a) -> m a
withCurId id f = do
    prevId <- liftInstBuilderState (gets curId)
    liftInstBuilderState $ modify $ \s -> s { curId = id }
    a <- f
    liftInstBuilderState $ modify $ \s -> s { curId = prevId }
    return a


prettyInst :: InstBuilderState -> IO ()
prettyInst state = do
    putStrLn "statements:"
    forM_ (Map.toList $ statements state) $ \(id, stmt) ->
        putStrLn $ show id ++ ": " ++ show stmt

    putStrLn "expressions:"
    forM_ (Map.toList $ expressions state) $ \(id, expr) ->
        putStrLn $ show id ++ ": " ++ show expr

    putStrLn "patterns:"
    forM_ (Map.toList $ patterns state) $ \(id, pat) ->
        putStrLn $ show id ++ ": " ++ show pat

    putStrLn "types:"
    forM_ (Map.toList $ types state) $ \(id, typ) ->
        putStrLn $ show id ++ ": " ++ show typ
