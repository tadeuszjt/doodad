{-# LANGUAGE FlexibleContexts #-}
module Annotate where

import qualified Data.Map as Map
import AST
import Monad
import Control.Monad.State
import qualified Type as T
import ASTResolved
import ASTMapper

-- 'Annotate takes an AST and annotates all expressions with a type variable using 'AExpr'.
-- This is the first step of the Hindley-Milner type inference algorithm.

class Annotate a where
    annotate :: BoM Int m => a -> m a

instance Annotate Param where
    --annotate (Param pos name T.Void) = Param pos name <$> genType
    annotate (Param pos name typ)    = return (Param pos name typ)

instance Annotate ASTResolved where
    annotate resolvedAst = do
        funcDefs <- mapM annotate (funcDefs resolvedAst)
        return $ resolvedAst
            { funcDefs = funcDefs 
            }

instance Annotate FuncBody where
    annotate funcBody = do
        params' <- mapM annotate (funcParams funcBody)
        args' <- mapM annotate (funcArgs funcBody)
        stmt' <- annotate (funcStmt funcBody)
        retty' <- return (funcRetty funcBody)
        return $ funcBody
            { funcParams = params'
            , funcArgs   = args'
            , funcRetty  = retty'
            , funcStmt   = stmt'
            }
        
mapper :: BoM Int m => Elem -> m Elem
mapper elem = case elem of
    ElemStmt _ -> return elem
    ElemExpr expr -> case expr of
        AExpr t e -> do
            let AExpr _ e' = e
            return $ ElemExpr $ AExpr t e'
        _ -> ElemExpr <$> annotateWithType expr
    ElemType _ -> return elem
    ElemPattern _ -> return elem


instance Annotate AST where
    annotate ast = do
        stmts <- mapM annotate (astStmts ast)
        return $ ast { astStmts = stmts }

instance Annotate Stmt where
    annotate stmt = mapStmt mapper stmt


annotateWithType :: BoM Int m => Expr -> m Expr
annotateWithType expr = do
    t <- genType
    return $ AExpr t expr

genType :: BoM Int m => m T.Type
genType = do
    i <- get
    put (i + 1)
    return (T.Type $ i)
