{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeChecker where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Fail

import qualified AST as S
import qualified Type as T

import Error


data TypeCheckerState
    = TypeCheckerState
        { 
        }
    deriving Show


initTypeCheckerState
    = TypeCheckerState
        {
        }


newtype TypeCheckerT m a
    = TypeCheckerT { getTypeChecker :: StateT TypeCheckerState (ExceptT CmpError m) a }
    deriving (Functor, Applicative, Monad, MonadState TypeCheckerState, MonadError CmpError)


runTypeCheckerT :: Monad m => TypeCheckerState -> TypeCheckerT m a -> m (Either CmpError (a, TypeCheckerState))
runTypeCheckerT typeCheckerState typeCheckerT =
    runExceptT $ runStateT (getTypeChecker typeCheckerT) typeCheckerState


class (MonadState TypeCheckerState m, MonadFail m) => MonadTypeChecker m

instance (MonadFail m) => MonadTypeChecker (TypeCheckerT m)

instance (Monad m, MonadFail m) => MonadFail (TypeCheckerT m) where
    fail s = throwError $ CmpError (TextPos 0 0 0, s)


data FlatAST
    = FlatAST
        { freshCount  :: Int
        , expressions :: Map.Map Int S.Expr
        , statements  :: Map.Map Int S.Stmt
        }
    deriving Show

initFlatAST
    = FlatAST
        { freshCount  = 0
        , expressions = Map.empty
        , statements  = Map.empty
        }


prettyFlatAST :: ([S.Stmt], FlatAST) -> IO ()
prettyFlatAST (top, flat) = do
    forM_ (Map.toList $ expressions flat) $ \(i, expr) -> putStrLn (show i ++ ": " ++ show expr)
    forM_ (Map.toList $ statements flat) $ \(i, stmt) -> putStrLn (show i ++ ": " ++ show stmt)
    putStrLn (show top)


flattenAST :: S.AST -> ([S.Stmt], FlatAST)
flattenAST ast = runState (mapM flatStmt ast) initFlatAST
    where
        addExpr :: S.Expr -> State FlatAST S.Expr
        addExpr expr = do
            count <- gets freshCount
            modify $ \s -> s
                { freshCount  = count + 1
                , expressions = Map.insert count expr (expressions s)
                }
            return (S.Expr count)

        addStmt :: S.Stmt -> State FlatAST S.Stmt
        addStmt stmt = do
            count <- gets freshCount
            modify $ \s -> s
                { freshCount = count + 1
                , statements = Map.insert count stmt (statements s)
                }
            return (S.Stmt count)

        flatStmt :: S.Stmt -> State FlatAST S.Stmt
        flatStmt stmt = addStmt =<< case stmt of
            S.Assign pos pattern expr          -> fmap (S.Assign pos pattern) (flatExpr expr)
            S.Set pos index expr               -> fmap (S.Set pos index) (flatExpr expr)
            S.CallStmt pos sym exprs           -> fmap (S.CallStmt pos sym) (mapM flatExpr exprs)
            S.Func pos sym params mretty stmts -> fmap (S.Func pos sym params mretty) (mapM flatStmt stmts)
            S.Extern pos sym params mretty     -> return stmt
            S.Return pos Nothing               -> return stmt
            S.Return pos (Just expr)           -> fmap (S.Return pos . Just) (flatExpr expr)
            S.While pos expr stmts             -> do
                expr' <- flatExpr expr
                fmap (S.While pos expr') (mapM flatStmt stmts)
            S.Switch pos expr cases -> do
                expr' <- flatExpr expr
                cases' <- forM cases $ \(pat, stmt) -> do
                    stmt' <- flatStmt stmt
                    return (pat, stmt')
                return (S.Switch pos expr' cases')
            _ -> error (show stmt)

        flatExpr :: S.Expr -> State FlatAST S.Expr
        flatExpr expr = addExpr =<< case expr of
            S.Cons c             -> return expr
            S.Conv pos typ exprs -> fmap (S.Conv pos typ) (mapM flatExpr exprs)
            S.Call pos sym exprs -> fmap (S.Call pos sym) (mapM flatExpr exprs) 
            S.Ident pos sym      -> return expr
            S.Len pos expr       -> fmap (S.Len pos) (flatExpr expr)
            S.Infix pos op a b   -> do
                a' <- flatExpr a
                b' <- flatExpr b
                return (S.Infix pos op a' b')
            S.Append pos a b     -> do
                a' <- flatExpr a
                b' <- flatExpr b
                return (S.Append pos a' b')
            S.Subscript pos a b     -> do
                a' <- flatExpr a
                b' <- flatExpr b
                return (S.Subscript pos a' b')

            _ -> error (show expr)
