module TupleDeleter where

import qualified Data.Map as Map
import Control.Monad.State

import AST
import ASTMapper
import ASTResolved
import Monad
import Type
import Error


tupleDeleterMapper :: Elem -> DoM ASTResolved Elem
tupleDeleterMapper elem = case elem of
    ElemType t -> ElemType <$> flattenType t
    _ -> return elem

deleteSingleTuples :: DoM ASTResolved ()
deleteSingleTuples = do
    funcDefs'  <- mapM (mapFuncBodyM tupleDeleterMapper) =<< gets funcDefs
    typeFuncs' <- mapM (\(ss, t) -> do { t' <- mapTypeM tupleDeleterMapper t; return (ss, t')}) =<< gets typeFuncs
    modify $ \s -> s { funcDefs = funcDefs', typeFuncs = typeFuncs' }

