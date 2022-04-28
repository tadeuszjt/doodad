{-# LANGUAGE FlexibleContexts #-}
module Annotate where

import AST
import Monad
import Control.Monad.State
import qualified Type as T

annotateAST :: BoM Int m => AST -> m AST
annotateAST ast = do
    stmts' <- mapM annotateStmt (astStmts ast)
    return $ ast { astStmts = stmts' }


annotateStmt :: BoM Int m => Stmt -> m Stmt
annotateStmt stmt = case stmt of
    FuncDef p s ps rt b -> FuncDef p s ps rt <$> annotateStmt b
    Block ss            -> Block <$> mapM annotateStmt ss
    Return p me         -> Return p <$> maybe (return Nothing) (fmap Just . annotateExpr) me
    Extern p n s ps rt  -> return stmt
    AppendStmt a        -> AppendStmt <$> annotateAppend a
    Print p es          -> Print p <$> mapM annotateExpr es
    Typedef p s a       -> return $ Typedef p s a
    CallStmt p s es     -> CallStmt p s <$> mapM annotateExpr es

    Assign p pat e      -> do
        pat' <- annotatePattern pat
        Assign p pat' <$> annotateExpr e

    Set p index e -> do
        index' <- annotateIndex index
        Set p index' <$> annotateExpr e

    If p c b elm        -> do
        c' <- annotateCondition c
        b' <- annotateStmt b
        elm' <- case elm of
            Nothing -> return Nothing
            Just st -> Just <$> annotateStmt st
        return $ If p c' b' elm'

    While p c b -> do
        c' <- annotateCondition c
        While p c' <$> annotateStmt b


annotateCondition :: BoM Int m => Condition -> m Condition
annotateCondition condition = case condition of
    CondExpr e -> CondExpr <$> annotateExpr e
    CondMatch p e -> do
        p' <- annotatePattern p
        e' <- annotateExpr e
        return $ CondMatch p' e'


annotatePattern :: BoM Int m => Pattern -> m Pattern
annotatePattern pattern = case pattern of
    PatIgnore p         -> return $ PatIgnore p
    PatIdent p s        -> return $ PatIdent p s
    PatLiteral e        -> PatLiteral <$> annotateExpr e
    PatTuple p pats     -> PatTuple p <$> mapM annotatePattern pats
    PatArray p pats     -> PatArray p <$> mapM annotatePattern pats

    PatGuarded p pat e -> do
        pat' <- annotatePattern pat
        PatGuarded p pat' <$> annotateExpr e


annotateAppend :: BoM Int m => Append -> m Append
annotateAppend append = case append of
    AppendIndex i -> AppendIndex <$> annotateIndex i
    AppendTable p a e -> do
        a' <- annotateAppend a
        AppendTable p a' <$> annotateExpr e
    AppendElem p a e -> do
        a' <- annotateAppend a
        AppendElem p a' <$> annotateExpr e


annotateIndex :: BoM Int m => Index -> m Index
annotateIndex index = case index of
    IndIdent p s -> return index

    IndArray p idx e -> do
        idx' <- annotateIndex idx
        IndArray p idx' <$> annotateExpr e
        
    _ -> fail $ "Cannot annotate: " ++ show index


annotateExpr :: BoM Int m => Expr -> m Expr
annotateExpr expr = annotateWithType =<< case expr of
    Ident p s -> return expr
    Char p c -> return expr
    Int p n -> return expr
    Float p f -> return expr
    String p s -> return expr
    Bool p b -> return expr

    Conv p s es -> Conv p s <$> mapM annotateExpr es
    Copy p e -> Copy p <$> annotateExpr e
    Len p e -> Len p <$> annotateExpr e
    Tuple p es -> Tuple p <$> mapM annotateExpr es
    Prefix p op e -> Prefix p op <$> annotateExpr e
    Table p ess -> Table p <$> mapM (mapM annotateExpr) ess
    Call p s es -> Call p s <$> mapM annotateExpr es

    Infix p op e1 e2 -> do
        e1' <- annotateExpr e1
        Infix p op e1' <$> annotateExpr e2

    Subscript p e1 e2 -> do
        e1' <- annotateExpr e1
        Subscript p e1' <$> annotateExpr e2

    Member p e s -> do
        e' <- annotateExpr e
        return $ Member p e' s

    _ -> error $ show expr


annotateWithType :: BoM Int m => Expr -> m Expr
annotateWithType expr = do
    i <- get
    put (i + 1)
    return $ AExpr (T.Type i) expr

