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

    Assign p pat e      -> do
        pat' <- annotatePattern pat
        Assign p pat' <$> annotateExpr e

    Set p index e -> do
        index' <- annotateIndex index
        e' <- annotateExpr e
        return $ Set p index' e'

    If p c b elm        -> do
        c' <- annotateCondition c
        b' <- annotateStmt b
        elm' <- case elm of
            Nothing -> return Nothing
            Just st -> Just <$> annotateStmt st
        return $ If p c' b' elm'

    For p si e gm b -> do
        e' <- annotateExpr e
        gm' <- case gm of
            Nothing -> return Nothing
            Just e  -> Just <$> annotateExpr e
        For p si e' gm' <$> annotateStmt b

    Switch p e cases -> do
        let (pats, stmts) = unzip cases
        pats' <- mapM annotatePattern pats
        stmts' <- mapM annotateStmt stmts
        e' <- annotateExpr e
        return $ Switch p e' (zip pats' stmts')

    While p c b -> do
        c' <- annotateCondition c
        b' <- annotateStmt b
        return $ While p c' b'

    CallStmt p i es -> do
        i' <- annotateIndex i
        es' <- mapM annotateExpr es
        return $ CallStmt p i' es'

    Print p es -> Print p <$> mapM annotateExpr es

    Typedef p s a -> return $ Typedef p s a

    _ -> fail $ "Cannot annotate: " ++ show stmt


annotateCondition :: BoM Int m => Condition -> m Condition
annotateCondition condition = case condition of
    CondExpr e -> CondExpr <$> annotateExpr e
    CondMatch p e -> do
        p' <- annotatePattern p
        e' <- annotateExpr e
        return $ CondMatch p' e'


annotatePattern :: BoM Int m => Pattern -> m Pattern
annotatePattern pattern = case pattern of
    PatIdent p s        -> return pattern
    PatLiteral e        -> PatLiteral <$> annotateExpr e
    PatTyped p typ pats -> PatTyped p typ <$> mapM annotatePattern pats
    PatTuple p pats     -> PatTuple p <$> mapM annotatePattern pats
    PatArray p pats     -> PatArray p <$> mapM annotatePattern pats
    PatIgnore p         -> return $ PatIgnore p

    PatSplit p pat1 pat2 -> do
        pat1' <- annotatePattern pat1
        pat2' <- annotatePattern pat2
        return $ PatSplit p pat1' pat2'

    PatSplitElem p pat1 pat2 -> do
        pat1' <- annotatePattern pat1
        pat2' <- annotatePattern pat2
        return $ PatSplitElem p pat1' pat2'


    PatGuarded p pat e -> do
        pat' <- annotatePattern pat
        e' <- annotateExpr e
        return $ PatGuarded p pat' e'


annotateAppend :: BoM Int m => Append -> m Append
annotateAppend append = case append of
    AppendIndex i -> AppendIndex <$> annotateIndex i
    AppendTable p a e -> do
        a' <- annotateAppend a
        e' <- annotateExpr e
        return $ AppendTable p a' e'
    AppendElem p a e -> do
        a' <- annotateAppend a
        e' <- annotateExpr e
        return $ AppendElem p a' e'


annotateIndex :: BoM Int m => Index -> m Index
annotateIndex index = case index of
    IndIdent p s -> return index
    IndArray p idx e -> do
        idx' <- annotateIndex idx
        e' <- annotateExpr e
        return $ IndArray p idx' e'
    _ -> fail $ "Cannot annotate: " ++ show index


annotateExpr :: BoM Int m => Expr -> m Expr
annotateExpr expr = do
    i <- get
    put (i + 1)
    return $ AExpr (T.Type i) expr

