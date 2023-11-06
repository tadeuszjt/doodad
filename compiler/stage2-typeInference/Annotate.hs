{-# LANGUAGE FlexibleContexts #-}
module Annotate where

import Monad
import Control.Monad.State
import Type
import AST
import ASTResolved
import ASTMapper

-- 'Annotate takes an AST and annotates all expressions with a type variable using 'AExpr'.
-- This is the first step of the Hindley-Milner type inference algorithm.
annotate :: BoM Int m => ASTResolved -> m ASTResolved
annotate resolvedAST = do 
    funcDefs <- mapM (mapFuncBodyM annotateMapper) (funcDefs resolvedAST)
    return $ resolvedAST { funcDefs = funcDefs }
        
annotateMapper :: BoM Int m => Elem -> m (Maybe Elem)
annotateMapper elem = case elem of
    ElemStmt _                     -> return (Just elem)
    ElemType _                     -> return (Just elem)
    ElemPattern _                  -> return (Just elem)
    ElemExpr (AExpr t (AExpr _ e)) -> return $ Just $ ElemExpr $ AExpr t e
    ElemExpr expr                  -> Just . ElemExpr <$> annotateWithType expr

annotateWithType :: BoM Int m => Expr -> m Expr
annotateWithType expr = do
    t <- genType
    return $ AExpr t expr

genType :: BoM Int m => m Type
genType = do
    i <- get
    put (i + 1)
    return (Type i)

-- DeAnnotate takes an AST and removes all unresolved type annotations.
deAnnotate :: BoM s m => ASTResolved -> m ASTResolved
deAnnotate resolvedAst = do
    funcDefs <- mapM (mapFuncBodyM deAnnotateMapper) (funcDefs resolvedAst)
    return $ resolvedAst { funcDefs = funcDefs }

deAnnotateMapper :: BoM s m => Elem -> m (Maybe Elem)
deAnnotateMapper elem = return $ case elem of
    ElemExpr (AExpr typ expr) | hasTypeVars typ -> Just $ ElemExpr expr
    ElemExpr _                                  -> Just elem
    ElemStmt _                                  -> Just elem
    ElemType _                                  -> Just elem
    ElemPattern _                               -> Just elem

hasTypeVars :: Type -> Bool
hasTypeVars typ = case typ of
    Type _         -> True
    Void           -> False
    t | isSimple t -> False
    Type.Tuple t   -> hasTypeVars t
    TypeApply s ts -> any (== True) (map hasTypeVars ts)
    Record ts      -> any (== True) (map hasTypeVars ts)
    Table t        -> hasTypeVars t
    Type.Range t   -> hasTypeVars t
    _ -> error (show typ)
