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



mapAST :: BoM s m => MapperFunc m -> AST -> m AST
mapAST f ast = do
    stmts' <- mapM (mapStmt f) (astStmts ast)
    return $ ast { astStmts = stmts' }

mapParam :: BoM s m => MapperFunc m -> Param -> m Param
mapParam f (AST.Param pos symbol typ) = withPos pos $ AST.Param pos symbol <$> (mapType f typ)
    

mapFuncBody :: BoM s m => MapperFunc m -> FuncBody -> m FuncBody
mapFuncBody f body = do
    funcParams' <- mapM (mapParam f) (funcParams body)
    funcArgs'   <- mapM (mapParam f) (funcArgs body)
    funcRetty'  <- mapType f (funcRetty body)
    funcStmt'   <- mapStmt f (funcStmt body)
    return $ FuncBody
        { funcTypeArgs = funcTypeArgs body
        , funcParams   = funcParams'
        , funcArgs     = funcArgs'
        , funcRetty    = funcRetty'
        , funcStmt     = funcStmt'
        }


mapFuncHeader :: BoM s m => MapperFunc m -> FuncHeader -> m FuncHeader
mapFuncHeader f header = do
    paramTypes' <- mapM (mapType f) (paramTypes header)
    argTypes'   <- mapM (mapType f) (argTypes header)
    returnType' <- mapType f (returnType header)
    return $ FuncHeader {
        typeArgs = typeArgs header,
        paramTypes = paramTypes',
        symbol     = symbol header,
        argTypes   = argTypes',
        returnType = returnType'
        }


mapStmt :: BoM s m => MapperFunc m -> Stmt -> m Stmt
mapStmt f stmt = withPos stmt $ do
    prevState <- get
    resm <- f . ElemStmt =<< case stmt of
        EmbedC pos s -> return $ EmbedC pos s
        Block stmts -> Block <$> mapM (mapStmt f) stmts
        ExprStmt expr -> ExprStmt <$> mapExpr f expr
        Return pos mexpr -> Return pos <$> maybe (return Nothing) (fmap Just . mapExpr f) mexpr

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

        Switch pos expr cases -> do
            expr' <- mapExpr f expr
            cases' <- forM cases $ \(pat, stmt) -> do
                pat' <- mapPattern f pat
                stmt' <- mapStmt f stmt
                return (pat', stmt')
            return $ Switch pos expr' cases'

        Data pos symbol typ mexpr -> do
            typ' <- mapType f typ
            mexpr' <- maybe (return Nothing) (fmap Just . mapExpr f) mexpr
            return $ Data pos symbol typ' mexpr'

        SetOp pos op expr1 expr2 -> do
            expr1' <- mapExpr f expr1
            expr2' <- mapExpr f expr2
            return $ SetOp pos op expr1' expr2'

        _ -> error (show stmt)
    case resm of
        Nothing -> put prevState >> return stmt
        Just (ElemStmt x) -> return x


mapExpr :: BoM s m => MapperFunc m -> Expr -> m Expr
mapExpr f expr = withPos expr $ do
    prevState <- get
    resm <- f . ElemExpr =<< case expr of
        AExpr typ expr -> do
            typ' <- mapType f typ
            expr' <- mapExpr f expr
            return $ AExpr typ' expr'

        Ident pos symbol    -> return $ Ident pos symbol
        AST.String pos s    -> return $ AST.String pos s
        AST.Int pos n       -> return $ AST.Int pos n
        AST.Bool pos b      -> return $ AST.Bool pos b
        AST.Tuple pos exprs -> AST.Tuple pos <$> mapM (mapExpr f) exprs
        RecordAccess pos expr -> RecordAccess pos <$> mapExpr f expr
        Construct pos symbol exprs -> Construct pos symbol <$> mapM (mapExpr f) exprs

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

        Subscript pos expr arg -> do
            expr' <- mapExpr f expr
            arg' <- mapExpr f arg
            return $ Subscript pos expr' arg'

        AST.Range pos mexpr mexpr1 mexpr2 -> do
            mexpr' <- maybe (return Nothing)  (fmap Just . mapExpr f) mexpr
            mexpr1' <- maybe (return Nothing) (fmap Just . mapExpr f) mexpr1 
            mexpr2' <- maybe (return Nothing) (fmap Just . mapExpr f) mexpr2 
            return $ AST.Range pos mexpr' mexpr1' mexpr2'


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
        PatLiteral expr          -> PatLiteral <$> mapExpr f expr
        PatTuple pos pats        -> PatTuple pos <$> mapM (mapPattern f) pats
        PatField pos symbol pats -> PatField pos symbol <$> mapM (mapPattern f) pats
        _ -> error (show pattern)
    case resm of
        Nothing -> put prevState >> return pattern
        Just (ElemPattern x) -> return x


mapType :: BoM s m => MapperFunc m -> Type -> m Type
mapType f typ = do
    prevState <- get
    resm <- f . ElemType =<< case typ of
        Void           -> return typ
        Type _         -> return typ
        t | isSimple t -> return typ
        Record ts      -> Record <$> mapM (mapType f) ts
        Table t        -> Table <$> mapType f t
        Type.Range t   -> Type.Range <$> mapType f t
        Type.Tuple t   -> Type.Tuple <$> mapType f t
        ADT ts         -> ADT <$> mapM (mapType f) ts
        TypeApply s ts -> TypeApply s <$> mapM (mapType f) ts
        _ -> error (show typ)
    case resm of
        Nothing -> put prevState >> return typ
        Just (ElemType x)  -> return x
