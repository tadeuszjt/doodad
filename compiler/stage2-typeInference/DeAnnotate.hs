{-# LANGUAGE FlexibleContexts #-}
module DeAnnotate where

import qualified Data.Map as Map
import AST
import Monad
import Control.Monad.State
import qualified Type as T
import ASTResolved
import ASTMapper

-- DeAnnotate takes an AST and removes all unresolved type annotations.

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
        return $ funcBody
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
    deAnnotate stmt = mapStmt mapper stmt


mapper :: BoM s m => Elem -> m (Maybe Elem)
mapper elem = case elem of
    ElemStmt _                                  -> return $ Just elem
    ElemExpr (AExpr typ expr) | hasTypeVars typ -> return $ Just $ ElemExpr expr
    ElemExpr (AExpr typ expr) | otherwise       -> return $ Just $ ElemExpr $ AExpr typ expr
    ElemExpr expr                               -> return $ Just $ ElemExpr expr
    ElemType _                                  -> return $ Just elem
    ElemPattern _                               -> return $ Just elem


hasTypeVars :: T.Type -> Bool
hasTypeVars typ = case typ of
    T.Type _         -> True
    T.Void           -> False
    t | T.isSimple t -> False
    T.Tuple t        -> hasTypeVars t
    T.TypeApply s ts -> any (== True) (map hasTypeVars ts)
    T.Record ts      -> any (== True) (map hasTypeVars ts)
    T.Table t        -> hasTypeVars t
    _ -> error (show typ)
    where
        fHasTypeVars :: T.AdtField -> Bool
        fHasTypeVars field = case field of
            T.FieldNull -> False
            T.FieldType t -> hasTypeVars t
            T.FieldCtor ts -> any (== True) (map hasTypeVars ts)
            _ -> error (show field)
