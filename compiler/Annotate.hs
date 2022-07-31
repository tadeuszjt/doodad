{-# LANGUAGE FlexibleContexts #-}
module Annotate where

import AST
import Monad
import Control.Monad.State
import qualified Type as T


class Annotate a where
    annotate :: BoM Int m => a -> m a


instance Annotate Param where
    annotate (Param pos name T.Void) = Param pos name <$> genType
    annotate (Param pos name typ)    = return (Param pos name typ)

instance Annotate AST where
    annotate ast = do
        stmts <- mapM annotate (astStmts ast)
        return $ ast { astStmts = stmts }

instance Annotate Stmt where
    annotate stmt = case stmt of
        FuncDef p s ps rt b -> do
            b' <- annotate b
            ps' <- mapM annotate ps
            rt' <- case rt of
                T.Void -> genType
                t      -> return t
            return $ FuncDef p s ps' rt' b'

        Block ss            -> Block <$> mapM annotate ss
        Return p me         -> Return p <$> maybe (return Nothing) (fmap Just . annotate) me

        AppendStmt a        -> AppendStmt <$> annotate a
        Print p es          -> Print p <$> mapM annotate es
        Typedef p s a       -> return $ Typedef p s a
        CallStmt p s es     -> CallStmt p s <$> mapM annotate es

        Assign p pat e      -> do
            pat' <- annotate pat
            Assign p pat' <$> annotate e

        Set p index e -> do
            index' <- annotate index
            Set p index' <$> annotate e

        If p c b elm        -> do
            c' <- annotate c
            b' <- annotate b
            elm' <- case elm of
                Nothing -> return Nothing
                Just st -> Just <$> annotate st
            return $ If p c' b' elm'

        While p c b -> do
            c' <- annotate c
            While p c' <$> annotate b

        Switch p e cases -> do
            e' <- annotate e
            cases' <- forM cases $ \(pat, stmt) -> do
                pat' <- annotate pat
                stmt' <- annotate stmt
                return (pat', stmt')
            return $ Switch p e' cases'

        For p symbol Nothing expr mcnd blk -> do
            expr' <- annotate expr
            blk' <- annotate blk
            t <- genType
            mcnd' <- maybe (return Nothing) (fmap Just . annotate) mcnd
            return $ For p symbol (Just t) expr' mcnd' blk'



instance Annotate Condition where
    annotate condition = case condition of
        CondExpr e -> CondExpr <$> annotate e
        CondMatch p e -> do
            p' <- annotate p
            e' <- annotate e
            return $ CondMatch p' e'


instance Annotate Pattern where
    annotate pattern = case pattern of
        PatIgnore p         -> return $ PatIgnore p
        PatIdent p s        -> return $ PatIdent p s
        PatLiteral e        -> PatLiteral <$> annotate e
        PatTuple p pats     -> PatTuple p <$> mapM annotate pats
        PatArray p pats     -> PatArray p <$> mapM annotate pats

        PatGuarded p pat e -> do
            pat' <- annotate pat
            PatGuarded p pat' <$> annotate e

        PatField p symbol pats -> PatField p symbol <$> mapM annotate pats

        PatAnnotated pat typ -> do
            pat' <- annotate pat
            return $ PatAnnotated pat' typ



instance Annotate Append where
    annotate append = case append of
        AppendIndex i -> AppendIndex <$> annotate i
        AppendTable p a e -> do
            a' <- annotate a
            AppendTable p a' <$> annotate e


instance Annotate Index where
    annotate index = case index of
        IndIdent p s -> return index

        IndArray p idx e -> do
            idx' <- annotate idx
            IndArray p idx' <$> annotate e
            
        _ -> fail $ "Cannot annotate: " ++ show index


instance Annotate Expr where
    annotate (AExpr t e) = do
        AExpr t' e' <- annotate e
        return (AExpr t e')
    annotate expr = annotateWithType =<< case expr of
        Ident p s -> return expr
        Char p c -> return expr
        Int p n -> return expr
        Float p f -> return expr
        String p s -> return expr
        Bool p b -> return expr
        Zero p -> return expr

        Conv p s es -> Conv p s <$> mapM annotate es
        Copy p e -> Copy p <$> annotate e
        Len p e -> Len p <$> annotate e
        Tuple p es -> Tuple p <$> mapM annotate es
        Prefix p op e -> Prefix p op <$> annotate e
        Table p ess -> Table p <$> mapM (mapM annotate) ess
        Call p s es -> Call p s <$> mapM annotate es
        UnsafePtr p e -> UnsafePtr p <$> annotate e

        Infix p op e1 e2 -> do
            e1' <- annotate e1
            Infix p op e1' <$> annotate e2

        Subscript p e1 e2 -> do
            e1' <- annotate e1
            Subscript p e1' <$> annotate e2

        Member p e s -> do
            e' <- annotate e
            return $ Member p e' s

        TupleIndex p e i -> do
            e' <- annotate e
            return $ TupleIndex p e' i

        Range pos e me1 me2 -> do
            e' <- annotate e
            me1' <- maybe (return Nothing) (fmap Just . annotate) me1
            me2' <- maybe (return Nothing) (fmap Just . annotate) me2
            return $ Range pos e' me1' me2'

        _ -> error $ show expr


annotateWithType :: BoM Int m => Expr -> m Expr
annotateWithType expr = do
    t <- genType
    return $ AExpr t expr

genType :: BoM Int m => m T.Type
genType = do
    i <- get
    put (i + 1)
    return (T.Type $ i + 1)
