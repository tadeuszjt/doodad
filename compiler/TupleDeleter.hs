{-# LANGUAGE FlexibleContexts #-}
module TupleDeleter where

import qualified Data.Map as Map
import Control.Monad.State

import AST
import ASTMapper
import ASTResolved
import Monad
import Type
import Error


tupleDeleterMapper :: BoM ASTResolved m => TypeDefs -> Elem -> m (Maybe Elem)
tupleDeleterMapper typeDefs elem = do
    return $ case elem of
        ElemType (Type.Tuple t) | definitelyIgnoresTuples typeDefs t -> Just (ElemType t)
        _                                                            -> Just elem

deleteSingleTuples :: BoM ASTResolved m => m ()
deleteSingleTuples = do
    typeDefs <- gets typeFuncs
    funcDefs'  <- mapM (mapFuncBody (tupleDeleterMapper typeDefs)) =<< gets funcDefs
    typeFuncs' <- mapM (\(ss, t) -> do { t' <- mapType (tupleDeleterMapper typeDefs) t; return (ss, t')}) =<< gets typeFuncs
    modify $ \s -> s { funcDefs = funcDefs', typeFuncs = typeFuncs' }

