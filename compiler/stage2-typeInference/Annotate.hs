module Annotate where

import Monad
import Control.Monad.State
import Type
import AST
import ASTResolved
import ASTMapper

-- 'Annotate takes an AST and annotates all expressions with a type variable using 'AExpr'.
-- This is the first step of the Hindley-Milner type inference algorithm.
annotate :: ASTResolved -> DoM Int ASTResolved
annotate resolvedAST = do 
    funcDefs <- mapM (mapFuncBodyM annotateMapper) (funcDefs resolvedAST)
    return $ resolvedAST { funcDefs = funcDefs }
        
annotateMapper :: Elem -> DoM Int Elem
annotateMapper elem = case elem of
    ElemStmt _                     -> return elem
    ElemType _                     -> return elem
    ElemExpr (AExpr t (AExpr _ e)) -> return $ ElemExpr $ AExpr t e
    ElemExpr expr                  -> do
        t <- genType
        return $ ElemExpr (AExpr t expr)
    ElemPattern (PatAnnotated (PatAnnotated p _) t) -> do
        return $ ElemPattern (PatAnnotated p t)
    ElemPattern pattern -> do
        t <- genType
        return $ ElemPattern (PatAnnotated pattern t)

genType :: DoM Int Type
genType = do
    i <- get
    put (i + 1)
    return (Type i)

-- DeAnnotate takes an AST and removes all unresolved type annotations.
deAnnotate :: ASTResolved -> DoM () ASTResolved
deAnnotate resolvedAst = do
    funcDefs <- mapM (mapFuncBodyM deAnnotateMapper) (funcDefs resolvedAst)
    return $ resolvedAst { funcDefs = funcDefs }

deAnnotateMapper :: Elem -> DoM () Elem
deAnnotateMapper elem = return $ case elem of
    ElemExpr (AExpr typ expr)          | hasTypeVars typ -> ElemExpr expr
    ElemPattern (PatAnnotated pat typ) | hasTypeVars typ -> ElemPattern pat
    _                                                    -> elem

hasTypeVars :: Type -> Bool
hasTypeVars typ = case typ of
    Type _         -> True
    Void           -> False
    t | isSimple t -> False
    Type.Tuple t   -> hasTypeVars t
    TypeApply s ts -> any (== True) (map hasTypeVars ts)
    Type.Record ts -> any (== True) (map hasTypeVars ts)
    Table t        -> hasTypeVars t
    Type.Range t   -> hasTypeVars t
    RecordApply t  -> hasTypeVars t
    _ -> error (show typ)
