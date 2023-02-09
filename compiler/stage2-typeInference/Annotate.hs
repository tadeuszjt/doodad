{-# LANGUAGE FlexibleContexts #-}
module Annotate where

import qualified Data.Map as Map
import AST
import Monad
import Control.Monad.State
import qualified Type as T
import States

-- 'Annotate takes an AST and annotates all expressions with a type variable using 'AExpr'.
-- This is the first step of the Hindley-Milner type inference algorithm.

class Annotate a where
    annotate :: BoM Int m => a -> m a


instance Annotate Param where
    annotate (Param pos name T.Void) = Param pos name <$> genType
    annotate (Param pos name typ)    = return (Param pos name typ)

instance Annotate ResolvedAst where
    annotate resolvedAst = do
        funcDefs <- mapM annotate (funcDefs resolvedAst)
        return $ resolvedAst
            { funcDefs = funcDefs 
            }

instance Annotate FuncBody where
    annotate funcBody = do
        params' <- mapM annotate (funcParams funcBody)
        args' <- mapM annotate (funcArgs funcBody)
        stmts' <- mapM annotate (funcStmts funcBody)
        retty' <- case (funcRetty funcBody) of
            T.Void -> genType
            t      -> return t
        return $ FuncBody
            { funcParams = params'
            , funcArgs   = args'
            , funcRetty  = retty'
            , funcStmts  = stmts'
            }
        

instance Annotate AST where
    annotate ast = do
        stmts <- mapM annotate (astStmts ast)
        return $ ast { astStmts = stmts }

instance Annotate Stmt where
    annotate stmt = case stmt of
        Block ss          -> Block <$> mapM annotate ss
        Return p me       -> Return p <$> maybe (return Nothing) (fmap Just . annotate) me
        Print p es        -> Print p <$> mapM annotate es
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
            elm' <- maybe (return Nothing) (fmap Just . annotate) elm
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

        Data p symbol typ mexpr -> Data p symbol typ <$> maybe (return Nothing) (fmap Just . annotate) mexpr



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
        Tuple p es -> Tuple p <$> mapM annotate es
        Prefix p op e -> Prefix p op <$> annotate e
        Initialiser p es -> Initialiser p <$> mapM annotate es

        Call pos ps ident es -> do
            ps' <- mapM annotate ps
            es' <- mapM annotate es
            return $ Call pos ps' ident es'

        Builtin pos ps ident es -> do
            ps' <- mapM annotate ps
            es' <- mapM annotate es
            return $ Builtin pos ps' ident es'

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
