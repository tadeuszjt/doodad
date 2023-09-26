{-# LANGUAGE FlexibleContexts #-}
module DeAnnotate where

import qualified Data.Map as Map
import AST
import Monad
import Control.Monad.State
import qualified Type as T
import ASTResolved

-- 'DeAnnotate takes an AST and annotates all expressions with a type variable using 'AExpr'.
-- This is the first step of the Hindley-Milner type inference algorithm.

class DeAnnotate a where
    deAnnotate :: BoM s m => a -> m a


instance DeAnnotate Param where
    --annotate (Param pos name T.Void) = Param pos name <$> genType
    deAnnotate (Param pos name typ) = return (Param pos name typ)

instance DeAnnotate ASTResolved where
    deAnnotate resolvedAst = do
        funcDefs <- mapM deAnnotate (funcDefs resolvedAst)
        return $ resolvedAst
            { funcDefs = funcDefs 
            }

instance DeAnnotate FuncBody where
    deAnnotate funcBody = do
        params' <- mapM deAnnotate (funcParams funcBody)
        args' <- mapM deAnnotate (funcArgs funcBody)
        stmt' <- deAnnotate (funcStmt funcBody)
        retty' <- return (funcRetty funcBody)
        return $ FuncBody
            { funcParams = params'
            , funcArgs   = args'
            , funcRetty  = retty'
            , funcStmt   = stmt'
            }
        

instance DeAnnotate AST where
    deAnnotate ast = do
        stmts <- mapM deAnnotate (astStmts ast)
        return $ ast { astStmts = stmts }

instance DeAnnotate Stmt where
    deAnnotate stmt = case stmt of
        EmbedC p s        -> return (EmbedC p s)
        Block ss          -> Block <$> mapM deAnnotate ss
        Return p me       -> Return p <$> maybe (return Nothing) (fmap Just . deAnnotate) me
        ExprStmt e        -> ExprStmt <$> deAnnotate e
        Const p s e       -> return $ Const p s e -- don't deAnnotate consts

        FuncDef p ps s as rt blk -> do
            ps' <- mapM deAnnotate ps
            as' <- mapM deAnnotate as
            blk' <- deAnnotate blk
            return $ FuncDef p ps' s as' rt blk'

        Assign p pat e      -> do
            pat' <- deAnnotate pat
            Assign p pat' <$> deAnnotate e

        SetOp p op index e -> do
            index' <- deAnnotate index
            SetOp p op index' <$> deAnnotate e

        AST.Typedef pos args symbol anno ->
            return $ AST.Typedef pos args symbol anno -- has no expressions

        If p c b elm        -> do
            c' <- deAnnotate c
            b' <- deAnnotate b
            elm' <- maybe (return Nothing) (fmap Just . deAnnotate) elm
            return $ If p c' b' elm'

        While p c b -> do
            c' <- deAnnotate c
            While p c' <$> deAnnotate b

        Switch p e cases -> do
            e' <- deAnnotate e
            cases' <- forM cases $ \(pat, stmt) -> do
                pat' <- deAnnotate pat
                stmt' <- deAnnotate stmt
                return (pat', stmt')
            return $ Switch p e' cases'

        For p expr mcnd blk -> do
            expr' <- deAnnotate expr
            blk' <- deAnnotate blk
            mcnd' <- maybe (return Nothing) (fmap Just . deAnnotate) mcnd
            return $ For p expr' mcnd' blk'

        Data p symbol typ mexpr -> Data p symbol typ <$> maybe (return Nothing) (fmap Just . deAnnotate) mexpr



instance DeAnnotate Pattern where
    deAnnotate pattern = case pattern of
        PatIgnore p         -> return $ PatIgnore p
        PatIdent p s        -> return $ PatIdent p s
        PatLiteral e        -> PatLiteral <$> deAnnotate e
        PatTuple p pats     -> PatTuple p <$> mapM deAnnotate pats
        PatArray p pats     -> PatArray p <$> mapM deAnnotate pats
        PatNull p           -> return $ PatNull p

        PatGuarded p pat e -> do
            pat' <- deAnnotate pat
            e' <- deAnnotate e
            return $ PatGuarded p pat' e'

        PatField p symbol pats -> PatField p symbol <$> mapM deAnnotate pats

        PatTypeField p typ pat -> PatTypeField p typ <$> deAnnotate pat
            
        PatAnnotated pat typ -> do
            pat' <- deAnnotate pat
            return $ PatAnnotated pat' typ


instance DeAnnotate Expr where
    deAnnotate expr = case expr of
        AExpr typ e | hasTypeVars typ -> deAnnotate' e
        AExpr typ e | otherwise       -> AExpr typ <$> deAnnotate' e
        e                             -> deAnnotate' e
        where
            deAnnotate' expr = case expr of
                Ident p s -> return expr
                Char p c -> return expr
                Int p n -> return expr
                Float p f -> return expr
                String p s -> return expr
                Bool p b -> return expr
                Null p -> return expr

                Conv p s es -> Conv p s <$> mapM deAnnotate es
                Tuple p es -> Tuple p <$> mapM deAnnotate es
                Prefix p op e -> Prefix p op <$> deAnnotate e
                Construct p symbol es -> Construct p symbol <$> mapM deAnnotate es

                Call pos ps ident es -> do
                    ps' <- mapM deAnnotate ps
                    es' <- mapM deAnnotate es
                    return $ Call pos ps' ident es'

                Builtin pos ps ident es -> do
                    ps' <- mapM deAnnotate ps
                    es' <- mapM deAnnotate es
                    return $ Builtin pos ps' ident es'

                Infix p op e1 e2 -> do
                    e1' <- deAnnotate e1
                    Infix p op e1' <$> deAnnotate e2

                Subscript p e1 e2 -> do
                    e1' <- deAnnotate e1
                    Subscript p e1' <$> deAnnotate e2

                Field p e s -> do
                    e' <- deAnnotate e
                    return $ Field p e' s

                Match pos expr pat -> do
                    expr' <- deAnnotate expr
                    pat' <- deAnnotate pat
                    return $ Match pos expr' pat'

                Range pos mexpr mexpr1 mexpr2 -> do
                    mexpr' <- maybe (return Nothing) (fmap Just . deAnnotate) mexpr
                    mexpr1' <- maybe (return Nothing) (fmap Just . deAnnotate) mexpr1
                    mexpr2' <- maybe (return Nothing) (fmap Just . deAnnotate) mexpr2
                    return $ Range pos mexpr' mexpr1' mexpr2'

                Array pos exprs -> do
                    Array pos <$> mapM deAnnotate exprs

                _ -> error $ show expr


hasTypeVars :: T.Type -> Bool
hasTypeVars typ = case typ of
    T.Type _         -> True
    T.Void           -> False
    t | T.isSimple t -> False
    T.ADT fs         -> any (== True) (map fHasTypeVars fs)
    T.Table ts       -> any (== True) (map hasTypeVars ts)
    T.Tuple ts       -> any (== True) (map hasTypeVars ts)
    T.Range t        -> hasTypeVars t
    T.Array n t      -> hasTypeVars t
    T.TypeApply s ts -> any (== True) (map hasTypeVars ts)
    _ -> error (show typ)
    where
        fHasTypeVars :: T.AdtField -> Bool
        fHasTypeVars field = case field of
            T.FieldNull -> False
            T.FieldType t -> hasTypeVars t
            T.FieldCtor ts -> any (== True) (map hasTypeVars ts)
            _ -> error (show field)
