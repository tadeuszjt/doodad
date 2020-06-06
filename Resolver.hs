{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Resolver where

import Control.Monad.State
import Control.Monad.Except hiding (void)
import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified SymTab
import qualified Type as T
import Error


type Symbol = String
type Name   = String


data ResolverState
    = ResolverState
        { nameSupply  :: Map.Map Name Int
        , symbolTable :: SymTab.SymTab Symbol Name
        , expressions :: Map.Map Name S.Expr
        }
    deriving Show


initResolverState
    = ResolverState
        { nameSupply  = Map.empty
        , symbolTable = SymTab.initSymTab
        , expressions = Map.empty
        }


newtype ResolverT m a
    = ResolverT { getResolver :: StateT ResolverState (ExceptT CmpError m) a }
    deriving (Functor, Applicative, Monad, MonadState ResolverState, MonadError CmpError)


runResolverT :: Monad m => ResolverState -> ResolverT m a -> m (Either CmpError (a, ResolverState))
runResolverT resolverState resolverT =
    runExceptT $ runStateT (getResolver resolverT) resolverState


class (MonadState ResolverState m, MonadFail m) => MonadResolver m

instance (MonadFail m) => MonadResolver (ResolverT m)

instance (Monad m, MonadFail m) => MonadFail (ResolverT m) where
    fail s = throwError $ CmpError (TextPos 0 0 0, s)


fresh :: MonadResolver m => Symbol -> m Name
fresh symbol = do
    names <- gets nameSupply
    let i = maybe 0 (+1) (Map.lookup symbol names)
    modify $ \s -> s { nameSupply = Map.insert symbol i names }
    return (symbol ++ "_" ++ show i)


lookupSym :: MonadResolver m => Symbol -> m Name
lookupSym symbol = do
    symTab <- gets symbolTable
    maybe (fail $ symbol ++ " doesn't exist") return (SymTab.lookup symbol symTab)


checkSymUndef :: MonadResolver m => Symbol -> m ()
checkSymUndef symbol = do
    symTab <- gets symbolTable
    case SymTab.lookup symbol [head symTab] of
        Just _  -> fail (symbol ++ " already defined")
        Nothing -> return ()


addSymDef :: MonadResolver m => Symbol -> Name -> m ()
addSymDef symbol name =
    modify $ \s -> s { symbolTable = SymTab.insert symbol name (symbolTable s) }


pushScope :: MonadResolver m => m ()
pushScope =
    modify $ \s -> s { symbolTable = SymTab.push (symbolTable s) }


popScope :: MonadResolver m => m ()
popScope  =
    modify $ \s -> s { symbolTable = SymTab.pop (symbolTable s) }


addExpr :: MonadResolver m => S.Expr -> m S.Expr
addExpr expr = do
    name <- fresh $ takeWhile (`elem` ['a'..'z']) $ map toLower (show expr)
    modify $ \s -> s { expressions = Map.insert name expr (expressions s) }
    return (S.Ident (TextPos 0 0 0) name)


resolveAST :: (MonadFail m) => S.AST -> m (Either CmpError (S.AST, Map.Map Name S.Expr))
resolveAST ast = do
    res <- runResolverT initResolverState (mapM resStmt ast)
    return $ fmap (\(f, s) -> (f, expressions s)) res


resPattern :: MonadResolver m => S.Pattern -> m S.Pattern
resPattern pattern = case pattern of
    S.PatIgnore pos       -> return pattern
    S.PatLiteral cons     -> return pattern
    S.PatIdent pos symbol -> do
        checkSymUndef symbol
        name <- fresh symbol
        addSymDef symbol name
        --modify $ \s -> s { expressions = Map.insert name (S.Ident pos name) (expressions s) }
        return (S.PatIdent pos name)
    _ -> fail (show pattern)


resIndex :: MonadResolver m => S.Index -> m S.Index
resIndex index = case index of
    S.IndIdent pos symbol -> fmap (S.IndIdent pos) (lookupSym symbol)


resType :: MonadResolver m => T.Type -> m T.Type
resType typ = case typ of
    T.Char             -> return typ
    T.Table Nothing ts -> fmap (T.Table Nothing) (mapM resType ts)
        

resParam :: MonadResolver m => S.Param -> m S.Param
resParam (S.Param pos symbol typ) = do
    checkSymUndef symbol
    name <- fresh symbol
    addSymDef symbol name
    typ' <- resType typ
    return (S.Param pos name typ')


resStmt :: MonadResolver m => S.Stmt -> m S.Stmt
resStmt stmt = case stmt of
    S.Assign pos pattern expr -> do
        resPat <- resPattern pattern
        resExp <- resExpr expr
        return (S.Assign pos resPat resExp)
    S.Set pos index expr -> do
        resInd <- resIndex index
        resExp <- resExpr expr
        return (S.Set pos resInd resExp)
    S.Print pos exprs -> fmap (S.Print pos) (mapM resExpr exprs)
    S.Block pos stmts -> do
        pushScope
        resStmts <- mapM resStmt stmts
        popScope
        return (S.Block pos resStmts)
    S.CallStmt pos symbol exprs -> do
        name <- lookupSym symbol
        fmap (S.CallStmt pos name) (mapM resExpr exprs)
    S.Return pos mexpr -> do
        fmap (S.Return pos) $ case mexpr of
            Nothing -> return Nothing
            Just ex -> fmap Just (resExpr ex)
    S.While pos cnd stmts -> do
        resCnd <- resExpr cnd
        pushScope
        resStmts <- mapM resStmt stmts
        pushScope
        return (S.While pos resCnd resStmts)
    S.Switch pos cnd cases -> do
        resCnd <- resExpr cnd
        pushScope
        resCases <- forM cases $ \(pat, stmt) -> do
            pat' <- resPattern pat
            stmt' <- resStmt stmt
            return (pat', stmt')
        popScope
        return (S.Switch pos resCnd resCases)
    S.Extern pos symbol params mretty -> do
        checkSymUndef symbol
        addSymDef symbol symbol
        pushScope
        mretty' <- maybe (return Nothing) (fmap Just . resType) mretty
        params' <- mapM resParam params
        popScope
        return (S.Extern pos symbol params' mretty')
    S.Func pos symbol params mretty stmts -> do
        checkSymUndef symbol
        name <- fresh symbol
        addSymDef symbol name
        pushScope
        params' <- mapM resParam params
        mretty' <- maybe (return Nothing) (fmap Just . resType) mretty
        stmts'  <- mapM resStmt stmts
        return (S.Func pos name params' mretty' stmts')
    _ -> fail (show stmt)


resExpr :: MonadResolver m => S.Expr -> m S.Expr
resExpr (S.Ident pos symbol) = fmap (S.Ident pos) (lookupSym symbol)
resExpr expr = case expr of
    S.Cons c           -> return expr
    S.Tuple pos exprs  -> fmap (S.Tuple pos) (mapM resExpr exprs)
    S.Array pos exprs  -> fmap (S.Array pos) (mapM resExpr exprs)
    S.Table pos exprss -> fmap (S.Table pos) (mapM (mapM resExpr) exprss)
    S.Len pos expr     -> fmap (S.Len pos) (resExpr expr)
    S.Append pos a b -> do
        a' <- resExpr a
        b' <- resExpr b
        return (S.Append pos a' b')
    S.Call pos symbol exprs -> do
        name <- lookupSym symbol
        fmap (S.Call pos name) (mapM resExpr exprs)
    S.Conv pos typ exprs  -> do
        typ' <- resType typ
        fmap (S.Conv pos typ') (mapM resExpr exprs)
    S.Subscript pos expr ind -> do
        expr' <- resExpr expr
        fmap (S.Subscript pos expr') (resExpr ind)
    S.Infix pos op a b    -> do
        a' <- resExpr a
        b' <- resExpr b
        return (S.Infix pos op a' b')
    _ -> fail (show expr)
