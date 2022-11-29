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
        FuncDef p ps s as rt b -> do
            ps' <- mapM annotate ps
            as' <- mapM annotate as
            b' <- annotate b
            rt' <- case rt of
                T.Void -> genType
                t      -> return t
            return $ FuncDef p ps' s as' rt' b'

        Block ss            -> Block <$> mapM annotate ss
        Return p me         -> Return p <$> maybe (return Nothing) (fmap Just . annotate) me

        Print p es          -> Print p <$> mapM annotate es
        Typedef p s a       -> return $ Typedef p s a
        ExprStmt e        -> ExprStmt <$> annotate e

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

        For p expr mcnd blk -> do
            expr' <- annotate expr
            blk' <- annotate blk
            mcnd' <- maybe (return Nothing) (fmap Just . annotate) mcnd
            return $ For p expr' mcnd' blk'

        Data p symbol typ -> do return $ Data p symbol typ



instance Annotate Pattern where
    annotate pattern = case pattern of
        PatIgnore p         -> return $ PatIgnore p
        PatIdent p s        -> return $ PatIdent p s
        PatLiteral e        -> PatLiteral <$> annotate e
        PatTuple p pats     -> PatTuple p <$> mapM annotate pats
        PatArray p pats     -> PatArray p <$> mapM annotate pats
        PatNull p           -> return $ PatNull p

        PatGuarded p pat e -> do
            pat' <- annotate pat
            PatGuarded p pat' <$> annotate e

        PatField p symbol pats -> PatField p symbol <$> mapM annotate pats

        PatTypeField p typ pat -> PatTypeField p typ <$> annotate pat
            

        PatAnnotated pat typ -> do
            pat' <- annotate pat
            return $ PatAnnotated pat' typ


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
        Null p -> return expr

        Conv p s es -> Conv p s <$> mapM annotate es
        Len p e -> Len p <$> annotate e
        Tuple p es -> Tuple p <$> mapM annotate es
        Prefix p op e -> Prefix p op <$> annotate e
        Array p es -> Array p <$> mapM annotate es
        Call p s es -> Call p s <$> mapM annotate es
        UnsafePtr p e -> UnsafePtr p <$> annotate e

        Infix p op e1 e2 -> do
            e1' <- annotate e1
            Infix p op e1' <$> annotate e2

        Subscript p e1 e2 -> do
            e1' <- annotate e1
            Subscript p e1' <$> annotate e2

        Field p e s -> do
            e' <- annotate e
            return $ Field p e' s

        TupleIndex p e i -> do
            e' <- annotate e
            return $ TupleIndex p e' i

        AST.ADT pos e -> AST.ADT pos <$> annotate e

        CallMember pos e ident es -> do
            e' <- annotate e
            es' <- mapM annotate es
            return $ CallMember pos e' ident es'

        Push pos e es -> do
            e' <- annotate e
            es' <- mapM annotate es
            return $ Push pos e' es'

        Pop pos e es -> do
            e' <- annotate e
            es' <- mapM annotate es
            return $ Pop pos e' es'

        Clear pos e -> do
            e' <- annotate e
            return $ Clear pos e'

        Delete pos expr1 expr2 -> do
            expr1' <- annotate expr1
            expr2' <- annotate expr2
            return $ Delete pos expr1' expr2'

        Match pos expr pat -> do
            expr' <- annotate expr
            pat' <- annotate pat
            return $ Match pos expr' pat'

        Range pos mexpr mexpr1 mexpr2 -> do
            mexpr' <- maybe (return Nothing) (fmap Just . annotate) mexpr
            mexpr1' <- maybe (return Nothing) (fmap Just . annotate) mexpr1
            mexpr2' <- maybe (return Nothing) (fmap Just . annotate) mexpr2
            return $ Range pos mexpr' mexpr1' mexpr2'

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
