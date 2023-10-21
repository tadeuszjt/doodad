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


mapper :: BoM ASTResolved m => Elem -> m (Maybe Elem)
mapper elem = do
    typeDefs <- gets typeFuncs
    case elem of
        ElemType t    -> return $ Just $ ElemType (flattenTuple typeDefs t)
        _             -> return (Just elem)

deleteSingleTuples :: BoM ASTResolved m => m ()
deleteSingleTuples = do
    funcDefs'  <- mapM (mapFuncBody mapper) =<< gets funcDefs
    typeFuncs' <- mapM (\(ss, t) -> do { t' <- mapType mapper t; return (ss, t')}) =<< gets typeFuncs
    modify $ \s -> s { funcDefs = funcDefs', typeFuncs = typeFuncs' }
