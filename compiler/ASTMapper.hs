module ASTMapper where

import Control.Monad.State
import Monad
import AST
import ASTResolved
import Type
import Error

data Elem
    = ElemStmt Stmt
    | ElemExpr Expr
    | ElemType Type
    | ElemPattern Pattern
    deriving (Show)

-- MapperFunc takes an ast element and modifies it using the BoM monad. when returning Nothing,
-- all changes will be discarded including for the members of the element.
type MapperFunc m = (Elem -> m (Maybe Elem))


mapParamM :: BoM s m => MapperFunc m -> Param -> m Param
mapParamM f (AST.Param pos symbol typ) = withPos pos $ AST.Param pos symbol <$> (mapTypeM f typ)
    

mapFuncBodyM :: BoM s m => MapperFunc m -> FuncBody -> m FuncBody
mapFuncBodyM f body = do
    funcParams' <- mapM (mapParamM f) (funcParams body)
    funcArgs'   <- mapM (mapParamM f) (funcArgs body)
    funcRetty'  <- mapTypeM f (funcRetty body)
    funcStmt'   <- mapStmtM f (funcStmt body)
    return $ FuncBody
        { funcTypeArgs = funcTypeArgs body
        , funcParams   = funcParams'
        , funcArgs     = funcArgs'
        , funcRetty    = funcRetty'
        , funcStmt     = funcStmt'
        }


mapFuncHeaderM :: BoM s m => MapperFunc m -> FuncHeader -> m FuncHeader
mapFuncHeaderM f header = do
    paramTypes' <- mapM (mapTypeM f) (paramTypes header)
    argTypes'   <- mapM (mapTypeM f) (argTypes header)
    returnType' <- mapTypeM f (returnType header)
    return $ FuncHeader {
        typeArgs = typeArgs header,
        paramTypes = paramTypes',
        symbol     = symbol header,
        argTypes   = argTypes',
        returnType = returnType'
        }


mapStmtM :: BoM s m => MapperFunc m -> Stmt -> m Stmt
mapStmtM f stmt = withPos stmt $ do
    prevState <- get
    resm <- f . ElemStmt =<< case stmt of
        Typedef _ _ _ _ -> return stmt -- ignored
        EmbedC pos s -> return $ EmbedC pos s
        Block stmts -> Block <$> mapM (mapStmtM f) stmts
        ExprStmt expr -> ExprStmt <$> mapExprM f expr
        Return pos mexpr -> Return pos <$> maybe (return Nothing) (fmap Just . mapExprM f) mexpr

        Assign pos pat expr -> do
            pat' <- mapPattern f pat
            expr' <- mapExprM f expr
            return $ Assign pos pat' expr'

        Increment pos expr -> do
            expr' <- mapExprM f expr
            return $ Increment pos expr'

        For pos expr mcnd blk -> do
            expr' <- mapExprM f expr
            mcnd' <- maybe (return Nothing) (fmap Just . (mapPattern f)) mcnd
            blk'  <- mapStmtM f blk
            return $ For pos expr' mcnd' blk'

        If pos expr true mfalse -> do
            expr' <- mapExprM f expr
            true' <- mapStmtM f true
            mfalse' <- maybe (return Nothing) (fmap Just . mapStmtM f) mfalse
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
            mexpr' <- maybe (return Nothing) (fmap Just . mapExprM f) mexpr
            return $ Data pos symbol typ' mexpr'

        SetOp pos op expr1 expr2 -> do
            expr1' <- mapExprM f expr1
            expr2' <- mapExprM f expr2
            return $ SetOp pos op expr1' expr2'

        _ -> error (show stmt)
    case resm of
        Nothing -> put prevState >> return stmt
        Just (ElemStmt x) -> return x


mapExprM :: BoM s m => MapperFunc m -> Expr -> m Expr
mapExprM f expr = withPos expr $ do
    prevState <- get
    resm <- f . ElemExpr =<< case expr of
        AExpr typ expr -> do
            typ' <- mapTypeM f typ
            expr' <- mapExprM f expr
            return $ AExpr typ' expr'

        Ident pos symbol    -> return $ Ident pos symbol
        AST.String pos s    -> return $ AST.String pos s
        AST.Int pos n       -> return $ AST.Int pos n
        AST.Bool pos b      -> return $ AST.Bool pos b
        AST.Tuple pos exprs -> AST.Tuple pos <$> mapM (mapExprM f) exprs
        RecordAccess pos expr -> RecordAccess pos <$> mapExprM f expr
        Construct pos symbol exprs -> Construct pos symbol <$> mapM (mapExprM f) exprs

        Field pos expr symbol -> do
            expr' <- mapExprM f expr
            return $ Field pos expr' symbol

        Call pos ps symbol es -> do
            ps' <- mapM (mapExprM f) ps
            es' <- mapM (mapExprM f) es
            return $ Call pos ps' symbol es'

        Builtin pos ps symbol es -> do
            ps' <- mapM (mapExprM f) ps
            es' <- mapM (mapExprM f) es
            return $ Builtin pos ps' symbol es'

        Infix pos op expr1 expr2 -> do
            expr1' <- mapExprM f expr1
            expr2' <- mapExprM f expr2
            return $ Infix pos op expr1' expr2'

        Subscript pos expr arg -> do
            expr' <- mapExprM f expr
            arg' <- mapExprM f arg
            return $ Subscript pos expr' arg'

        AST.Range pos mexpr mexpr1 mexpr2 -> do
            mexpr' <- maybe (return Nothing)  (fmap Just . mapExprM f) mexpr
            mexpr1' <- maybe (return Nothing) (fmap Just . mapExprM f) mexpr1 
            mexpr2' <- maybe (return Nothing) (fmap Just . mapExprM f) mexpr2 
            return $ AST.Range pos mexpr' mexpr1' mexpr2'

        AST.Match pos expr pattern -> do
            expr' <- mapExprM f expr
            pattern' <- mapPattern f pattern
            return $ AST.Match pos expr' pattern'

        _ -> error (show expr)
    case resm of
        Nothing -> put prevState >> return expr
        Just (ElemExpr x) -> return x


mapPattern :: BoM s m => MapperFunc m -> Pattern -> m Pattern
mapPattern f pattern = withPos pattern $ do
    prevState <- get
    resm <- f . ElemPattern =<< case pattern of
        PatIdent pos symbol      -> return pattern
        PatIgnore pos            -> return pattern
        PatLiteral expr          -> PatLiteral <$> mapExprM f expr
        PatTuple pos pats        -> PatTuple pos <$> mapM (mapPattern f) pats
        PatField pos symbol pats -> PatField pos symbol <$> mapM (mapPattern f) pats
        PatRecord pos pats       -> PatRecord pos <$> mapM (mapPattern f) pats
        _ -> error (show pattern)
    case resm of
        Nothing -> put prevState >> return pattern
        Just (ElemPattern x) -> return x


mapTypeM :: BoM s m => MapperFunc m -> Type -> m Type
mapTypeM f typ = do
    prevState <- get
    resm <- f . ElemType =<< case typ of
        Type.U8        -> return typ
        Type.I8        -> return typ
        Type.I16       -> return typ
        Type.I32       -> return typ
        Type.I64       -> return typ
        Type.F32       -> return typ
        Type.F64       -> return typ
        Type.Bool      -> return typ
        Type.String    -> return typ
        Type.Char      -> return typ
        Type _         -> return typ
        Record ts      -> Record <$> mapM (mapTypeM f) ts
        Type.Tuple t   -> Type.Tuple <$> mapTypeM f t
        Table t        -> Table <$> mapTypeM f t
        TypeApply s ts -> TypeApply s <$> mapM (mapTypeM f) ts
        Type.Range t   -> Type.Range <$> mapTypeM f t
        ADT ts         -> ADT <$> mapM (mapTypeM f) ts
        Void           -> return typ
        _ -> error (show typ)
    case resm of
        Just (ElemType x) -> return x
        Nothing           -> put prevState >> return typ

