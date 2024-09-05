{-# LANGUAGE FlexibleInstances #-}
module InstBuilder where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity

import AST
import Monad


type ID = Int

globalId = 0

data InstBuilderState = InstBuilderState
    { statements    :: Map.Map ID Stmt
    , expressions   :: Map.Map ID Expr
    , patterns      :: Map.Map ID Pattern
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


unbuildExpr :: InstBuilderState-> Expr -> DoM () Expr
unbuildExpr state expression = do
    expr <- case expression of
        Expr id -> let Just expr = Map.lookup id (expressions state) in return expr
        expr    -> return expr
    case expr of
        Call pos typ exprs -> Call pos typ <$> mapM (unbuildExpr state) exprs
        Reference pos expr -> Reference pos <$> unbuildExpr state expr
        Ident pos symbol   -> return expr
        Int pos n          -> return expr
        Float pos f        -> return expr
        AST.Bool pos b     -> return expr
        AST.Char pos c     -> return expr
        AST.String pos s   -> return expr
        AExpr t e          -> AExpr t <$> unbuildExpr state e
        x -> error (show x)


unbuildPattern :: InstBuilderState-> Pattern -> DoM () Pattern
unbuildPattern state pattern = do
    pat <- case pattern of
        Pattern id -> let Just pat = Map.lookup id (patterns state) in return pat
        pat    -> return pat
    case pat of
        PatIdent pos symbol -> return pat
        PatAnnotated p typ  -> do
            p' <- unbuildPattern state p
            return (PatAnnotated p' typ)
        x -> error (show x)


unbuildInst :: InstBuilderState -> ID -> DoM () Stmt
unbuildInst instState id = do
    let Just stmt = Map.lookup id (statements instState)
    case stmt of
        Block stmts -> fmap Block $ forM stmts $ \(Stmt id) -> unbuildInst instState id
        Return pos mexpr -> Return pos <$> traverse (unbuildExpr instState) mexpr
        ExprStmt expr    -> ExprStmt <$> unbuildExpr instState expr
        EmbedC _ _ _     -> return stmt
        Let pos pat mexpr Nothing  -> do
            pat' <- unbuildPattern instState pat
            mexpr' <- traverse (unbuildExpr instState) mexpr
            return $ Let pos pat' mexpr' Nothing
        Assign pos s expr -> Assign pos s <$> unbuildExpr instState expr

        If pos expr (Stmt trueId) mfalse -> do
            expr' <- unbuildExpr instState expr
            true' <- unbuildInst instState trueId
            false' <- case mfalse of
                Nothing -> return Nothing
                Just (Stmt id) -> fmap Just $ unbuildInst instState id

            return $ If pos expr' true' false'

        While pos expr (Stmt id) -> do
            expr' <- unbuildExpr instState expr
            While pos expr' <$> unbuildInst instState id


        x -> error (show x)
