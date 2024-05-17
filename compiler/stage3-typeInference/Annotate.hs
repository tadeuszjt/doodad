module Annotate where

import Monad
import Control.Monad.State
import Type
import AST
import ASTResolved
import ASTMapper

-- 'Annotate takes an AST and annotates all expressions with a type variable using 'AExpr'.
-- This is the first step of the Hindley-Milner type inference algorithm.
annotateFunc :: Func -> DoM Int Func
annotateFunc func = do
    mapFuncM annotateMapper func

        
annotateMapper :: Elem -> DoM Int Elem
annotateMapper elem = case elem of
    ElemStmt _                     -> return elem
    ElemType (Type 0)              -> ElemType <$> genType
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
    return $ Type (i + 1)

-- DeAnnotate takes an AST and removes all unresolved type annotations.
deAnnotateFunc :: Func -> DoM () Func
deAnnotateFunc func = do
    mapFuncM deAnnotateMapper func


deAnnotateMapper :: Elem -> DoM () Elem
deAnnotateMapper elem = return $ case elem of
    ElemType (Type n)                                    -> ElemType (Type 0)
    ElemExpr (AExpr typ expr)          | hasTypeVars typ -> ElemExpr expr
    ElemPattern (PatAnnotated pat typ) | hasTypeVars typ -> ElemPattern pat
    _                                                    -> elem


hasTypeVars :: Type -> Bool
hasTypeVars typ = case typ of
    Type _         -> True
    Apply t ts     -> any hasTypeVars (t : ts)
    _              -> False
