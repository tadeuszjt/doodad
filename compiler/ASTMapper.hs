{-# LANGUAGE FlexibleContexts #-}
module ASTMapper where

import Control.Monad.State
import Control.Monad.Except
import AST
import ASTResolved
import Type
import Error
import Monad

data Elem
    = ElemStmt Stmt
    | ElemExpr Expr
    | ElemType Type
    | ElemPattern Pattern
    deriving (Show)

type MapperFunc s = (Elem -> DoM s Elem)

mapParamM :: MapperFunc s -> Param -> DoM s Param
mapParamM f (AST.Param pos symbol typ) = withPos pos $ AST.Param pos symbol <$> (mapTypeM f typ)
mapParamM f (AST.RefParam pos symbol typ) = withPos pos $ AST.RefParam pos symbol <$> (mapTypeM f typ)


mapRettyM :: MapperFunc s -> Retty -> DoM s Retty
mapRettyM f (AST.Retty t) = AST.Retty <$> mapTypeM f t
mapRettyM f (AST.RefRetty t) = AST.RefRetty <$> mapTypeM f t
    

mapFuncBodyM :: MapperFunc s -> FuncBody -> DoM s FuncBody
mapFuncBodyM f body = do
    funcParams' <- mapM (mapParamM f) (funcParams body)
    funcArgs'   <- mapM (mapParamM f) (funcArgs body)
    funcRetty'  <- mapRettyM f (funcRetty body)
    funcStmt'   <- mapStmtM f (funcStmt body)
    return $ FuncBody
        { funcGenerics = funcGenerics body
        , funcParams   = funcParams'
        , funcArgs     = funcArgs'
        , funcRetty    = funcRetty'
        , funcStmt     = funcStmt'
        }


mapStmtM :: MapperFunc s -> Stmt -> DoM s Stmt
mapStmtM f stmt = withPos stmt $ do
    res <- f . ElemStmt =<< case stmt of
        Typedef _ _ _ _ -> return stmt -- ignored
        EmbedC pos s -> return $ EmbedC pos s
        Block stmts -> Block <$> mapM (mapStmtM f) stmts
        ExprStmt expr -> ExprStmt <$> mapExprM f expr
        Return pos mexpr -> Return pos <$> traverse (mapExprM f) mexpr

        Let pos pat mexpr mblk -> do
            pat' <- mapPattern f pat
            mexpr' <- traverse (mapExprM f) mexpr
            mblk' <- traverse (mapStmtM f) mblk
            return $ Let pos pat' mexpr' mblk'

        For pos expr mcnd blk -> do
            expr' <- mapExprM f expr
            mcnd' <- traverse (mapPattern f) mcnd
            blk'  <- mapStmtM f blk
            return $ For pos expr' mcnd' blk'

        While pos cnd blk -> do
            cnd' <- mapExprM f cnd
            blk' <- mapStmtM f blk
            return $ While pos cnd' blk'

        If pos expr true mfalse -> do
            expr' <- mapExprM f expr
            true' <- mapStmtM f true
            mfalse' <- traverse (mapStmtM f) mfalse
            return $ If pos expr' true' mfalse'

        Switch pos expr cases -> do
            expr' <- mapExprM f expr
            cases' <- forM cases $ \(pat, stmt) -> do
                pat' <- mapPattern f pat
                stmt' <- mapStmtM f stmt
                return (pat', stmt')
            return $ Switch pos expr' cases'

        Data pos symbol typ mexpr -> do
            typ' <- mapTypeM f typ
            mexpr' <- traverse (mapExprM f) mexpr
            return $ Data pos symbol typ' mexpr'

        SetOp pos op expr1 expr2 -> do
            expr1' <- mapExprM f expr1
            expr2' <- mapExprM f expr2
            return $ SetOp pos op expr1' expr2'

        _ -> error (show stmt)
    case res of
        ElemStmt x -> return x
        _          -> error "result wasn't ElemStmt"


mapExprM :: MapperFunc s -> Expr -> DoM s Expr
mapExprM f expr = withPos expr $ do
    res <- f . ElemExpr =<< case expr of
        AExpr typ expr -> do
            typ' <- mapTypeM f typ
            expr' <- mapExprM f expr
            return $ AExpr typ' expr'

        Ident pos symbol      -> return $ Ident pos symbol
        AST.String pos s      -> return $ AST.String pos s
        AST.Int pos n         -> return $ AST.Int pos n
        AST.Float pos f       -> return $ AST.Float pos f
        AST.Bool pos b        -> return $ AST.Bool pos b
        AST.Char pos c        -> return $ AST.Char pos c
        AST.Tuple pos exprs   -> AST.Tuple pos <$> mapM (mapExprM f) exprs
        Call pos symbol exprs -> Call pos symbol <$> mapM (mapExprM f) exprs

        Field pos expr n -> do
            expr' <- mapExprM f expr
            return $ Field pos expr' n

        Builtin pos symbol es -> do
            es' <- mapM (mapExprM f) es
            return (Builtin pos symbol es')

        AST.Match pos expr pattern -> do
            expr' <- mapExprM f expr
            pattern' <- mapPattern f pattern
            return $ AST.Match pos expr' pattern'

        AST.Reference pos expr -> do
            expr' <- mapExprM f expr
            return $ AST.Reference pos expr'

        _ -> error (show expr)
    case res of
        ElemExpr x -> return x
        _          -> error "result wasn't ElemExpr"


mapPattern :: MapperFunc s -> Pattern -> DoM s Pattern
mapPattern f pattern = withPos pattern $ do
    res <- f . ElemPattern =<< case pattern of
        PatIdent pos symbol      -> return pattern
        PatIgnore pos            -> return pattern
        PatLiteral expr          -> PatLiteral <$> mapExprM f expr
        PatTuple pos pats        -> PatTuple pos <$> mapM (mapPattern f) pats

        PatTypeField pos typ pats -> do
            typ' <- mapTypeM f typ
            PatTypeField pos typ' <$> mapM (mapPattern f) pats

        PatGuarded pos pat expr  -> do
            pat' <- mapPattern f pat
            expr' <- mapExprM f expr
            return $ PatGuarded pos pat' expr'
        PatAnnotated pat typ     -> do
            pat' <- mapPattern f pat
            typ' <- mapTypeM f typ
            return $ PatAnnotated pat' typ'
        PatField pos symbol pat -> PatField pos symbol <$> mapPattern f pat
        _ -> error (show pattern)
    case res of
        ElemPattern x -> return x
        _             -> error "result wasn't ElemPattern"


mapTypeM :: MapperFunc s -> Type -> DoM s Type
mapTypeM f typ = do
    res <- f . ElemType =<< case typ of
        x | isSimple x -> return typ
        Type _         -> return typ
        Void           -> return typ
        Size _         -> return typ
        TypeApply s ts -> TypeApply s <$> mapM (mapTypeM f) ts
        Slice t        -> Slice <$> mapTypeM f t
        _ -> error (show typ)
    case res of
        ElemType x -> return x
        _          -> error "result wasn't ElemType"

