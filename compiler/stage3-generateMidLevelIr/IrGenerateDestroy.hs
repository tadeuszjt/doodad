{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IrGenerateDestroy where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import Type
import Ir
import ASTResolved
import Error
import IrGenerate


data DestroyState = DestroyState
    { 
    }


initDestroyState = DestroyState
    {
    }


newtype IrGenerateDestroy a = IrGenerateDestroy
    { unIrGenerateDestroy :: StateT DestroyState (StateT FuncIr (StateT ASTResolved (Except Error))) a }
    deriving (Functor, Applicative, Monad, MonadState DestroyState, MonadError Error)


instance MonadFail IrGenerateDestroy where
    fail = throwError . ErrorStr


instance MonadFuncIr IrGenerateDestroy where
    liftFuncIrState (StateT s) = IrGenerateDestroy $ lift $ StateT (pure . runIdentity . s)


liftAstState :: StateT ASTResolved (Except Error) a -> IrGenerateDestroy a
liftAstState = IrGenerateDestroy . lift . lift


runIrGenerateDestroy :: DestroyState -> FuncIr -> IrGenerateDestroy a
    -> StateT ASTResolved (Except Error) (a, FuncIr)
runIrGenerateDestroy destroyState funcIr f = do
    ((a, _), funcIr) <- runStateT (runStateT (unIrGenerateDestroy f) destroyState) funcIr
    return (a, funcIr)


-- add destroy for all top instances
irGenerateDestroyPass :: StateT ASTResolved (Except Error) ()
irGenerateDestroyPass = do
    mapM_ irGenerateDestroyInst =<< gets (Set.toList . instantiationsTop)


-- add destroy for specific instance
irGenerateDestroyInst :: Type -> StateT ASTResolved (Except Error) ()
irGenerateDestroyInst instType = do
    funcIr <- gets (fromJust . Map.lookup instType . instantiations)
    ((), funcIr') <- runIrGenerateDestroy initDestroyState initFuncIr (destroyIr funcIr)
    modify $ \s -> s { instantiations = Map.insert instType funcIr' (instantiations s) }
    

destroyIr :: FuncIr -> IrGenerateDestroy ()
destroyIr funcIr = do
    liftFuncIrState $ put $ funcIr
        { irStmts = Map.singleton 0 (Block [])
        , irCurrentId = 0
        }

    let Block ids = fromJust $ Map.lookup 0 (irStmts funcIr)
    mapM_ (destroyStmt funcIr) ids


destroyStmt :: FuncIr -> Ir.ID -> IrGenerateDestroy ()
destroyStmt funcIr id = case fromJust $ Map.lookup id (irStmts funcIr) of
    Block ids -> do
        appendStmtWithId id (Block [])
        withCurrentId id $ mapM_ (destroyStmt funcIr) ids


    x -> error (show x)

