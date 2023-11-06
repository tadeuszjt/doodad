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


tupleDeleterMapper :: BoM ASTResolved m => Elem -> m Elem
tupleDeleterMapper elem = case elem of
    ElemType (Type.Tuple t) -> do
        b <- definitelyIgnoresTuples t
        case b of
            True  -> return (ElemType t)
            False -> return elem
    _ -> return elem

deleteSingleTuples :: BoM ASTResolved m => m ()
deleteSingleTuples = do
    funcDefs'  <- mapM (mapFuncBodyM tupleDeleterMapper) =<< gets funcDefs
    typeFuncs' <- mapM (\(ss, t) -> do { t' <- mapTypeM tupleDeleterMapper t; return (ss, t')}) =<< gets typeFuncs
    modify $ \s -> s { funcDefs = funcDefs', typeFuncs = typeFuncs' }

