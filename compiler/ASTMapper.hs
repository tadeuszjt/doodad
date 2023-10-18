module ASTMapper where

import Monad
import AST
import Type
import Error

data Elem
    = ElemStmt Stmt
    | ElemExpr Expr
    | ElemType Type
    | ElemPattern Pattern
    deriving (Show)

type MapperFunc m = (Elem -> m Elem)

mapStmt :: BoM s m => MapperFunc m -> Stmt -> m Stmt
mapStmt f stmt = withPos stmt $ do
    ElemStmt x <- f . ElemStmt =<< case stmt of
        EmbedC pos s -> return $ EmbedC pos s
        Block stmts -> Block <$> mapM (mapStmt f) stmts
        ExprStmt expr -> ExprStmt <$> mapExpr f expr
        Return pos mexpr -> Return <$> maybe (return Nothing) (fmap Just . mapExpr f) mexpr

        Assign pos pat expr -> do
            pat' <- mapPattern f pat
            expr' <- mapExpr f expr
            return $ Assign pos pat' expr'

        Increment pos expr -> do
            expr' <- mapExpr f expr
            return $ Increment pos expr'

        For pos expr mcnd blk -> do
            expr' <- mapExpr f expr
            mcnd' <- maybe (return Nothing) (fmap Just . (mapPattern f)) mcnd
            blk'  <- mapStmt f blk
            return $ For pos expr' mcnd' blk'

        If pos expr true mfalse -> do
            expr' <- mapExpr f expr
            true' <- mapStmt f true
            mfalse' <- maybe (return Nothing) (fmap Just . mapStmt f) mfalse
            return $ If pos expr' true' mfalse'

        Data pos symbol typ mexpr -> do
            typ' <- mapType f typ
            mexpr' <- maybe (return Nothing) (fmap Just . mapExpr f) mexpr
            return $ Data pos symbol typ' mexpr'

        SetOp pos op expr1 expr2 -> do
            expr1' <- mapExpr f expr1
            expr2' <- mapExpr f expr2
            return $ SetOp pos op expr1' expr2'

        _ -> error (show stmt)
    return x


mapExpr :: BoM s m => MapperFunc m -> Expr -> m Expr
mapExpr f expr = withPos expr $ do
    ElemExpr x <- f . ElemExpr =<< case expr of
        AExpr typ expr -> do
            typ' <- mapType f typ
            expr' <- mapExpr f expr
            return $ AExpr typ' expr'

        Ident pos symbol    -> return $ Ident pos symbol
        AST.String pos s    -> return $ AST.String pos s
        AST.Int pos n       -> return $ AST.Int pos n
        AST.Bool pos b      -> return $ AST.Bool pos b
        AST.Tuple pos exprs -> AST.Tuple <$> mapM (mapExpr f) exprs

        Field pos expr symbol -> do
            expr' <- mapExpr f expr
            return $ Field pos expr' symbol

        Call pos ps symbol es -> do
            ps' <- mapM (mapExpr f) ps
            es' <- mapM (mapExpr f) es
            return $ Call pos ps' symbol es'

        Builtin pos ps symbol es -> do
            ps' <- mapM (mapExpr f) ps
            es' <- mapM (mapExpr f) es
            return $ Builtin pos ps' symbol es'

        Infix pos op expr1 expr2 -> do
            expr1' <- mapExpr f expr1
            expr2' <- mapExpr f expr2
            return $ Infix pos op expr1' expr2'

        Subscript pos expr mexpr -> do
            expr' <- mapExpr f expr
            mexpr' <- maybe (return Nothing) (fmap Just . mapExpr f) mexpr
            return $ Subscript pos expr' mexpr'

        _ -> error (show expr)
    return x


mapPattern :: BoM s m => MapperFunc m -> Pattern -> m Pattern
mapPattern f pattern = withPos pattern $ do
    ElemPattern x <- f . ElemPattern =<< case pattern of
        PatIdent pos symbol -> return $ PatIdent pos symbol
        PatTuple pos pats   -> PatTuple pos <$> mapM (mapPattern f) pats
        _ -> error (show pattern)
    return x


mapType :: BoM s m => MapperFunc m -> Type -> m Type
mapType f t = do
    ElemType x <- f $ ElemType t
    return x
