module FuncIrUnused where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
--import Control.Monad.IO.Class
import Data.Maybe

import IR
import Monad
import ASTResolved
import Symbol
import Type
import FindFunc
import qualified AST as S


data FuncIrUnusedState = FuncIrUnusedState
    { funcIr       :: FuncIR
    , astResolved  :: ASTResolved
    , hasReturned  :: Bool
    }


initFuncIrUnusedState ast = FuncIrUnusedState
    { funcIr      = initFuncIr
    , astResolved = ast
    , hasReturned = False
    }


liftFuncIr :: DoM FuncIR a -> DoM FuncIrUnusedState a
liftFuncIr f = do
    fn <- gets funcIr
    (a, fn') <- runDoMExcept fn f
    modify $ \s -> s { funcIr = fn' }
    return a



funcIrUnused :: FuncIR -> DoM FuncIrUnusedState ()
funcIrUnused func = do
    -- copy types from old map
    liftFuncIr $ modify $ \s -> s { irTypes = irTypes func }
    -- generate new ids from here
    liftFuncIr $ modify $ \s -> s { irIdSupply = irIdSupply func }

    void $ processStmt func 0


withCurrentId :: ID -> DoM FuncIrUnusedState a -> DoM FuncIrUnusedState a
withCurrentId id f = do
    oldId <- liftFuncIr (gets irCurrentId)
    liftFuncIr $ modify $ \s -> s { irCurrentId = id }
    a <- f
    liftFuncIr $ modify $ \s -> s { irCurrentId = oldId }
    modify $ \s -> s { hasReturned = False }
    return a


processStmt :: FuncIR -> ID -> DoM FuncIrUnusedState ()
processStmt funcIr id = do
    hasReturned <- gets hasReturned
    unless hasReturned $ do
        let stmt = irStmts funcIr Map.! id in case stmt of
            Block ids -> do
                void $ liftFuncIr $ appendStmtWithId id (Block [])
                withCurrentId id $ do
                    mapM_ (processStmt funcIr) ids

            Loop ids -> do
                void $ liftFuncIr $ appendStmtWithId id (Loop [])
                withCurrentId id $ do
                    mapM_ (processStmt funcIr) ids

            If cnd ids -> do
                void $ liftFuncIr $ appendStmtWithId id (If cnd [])
                withCurrentId id $ do
                    mapM_ (processStmt funcIr) ids

            Else ids -> do
                void $ liftFuncIr $ appendStmtWithId id (Else [])
                withCurrentId id $ do
                    mapM_ (processStmt funcIr) ids

            Break -> do
                modify $ \s -> s { hasReturned = True }
                void $ liftFuncIr $ appendStmtWithId id stmt

            EmbedC _ _ ->
                void $ liftFuncIr (appendStmtWithId id stmt)

            Return arg -> do
                modify $ \s -> s { hasReturned = True }
                void $ liftFuncIr (appendStmtWithId id stmt)

            ReturnVoid -> do
                modify $ \s -> s { hasReturned = True }
                void $ liftFuncIr (appendStmtWithId id stmt)

            SSA _ _ _ ->
                void $ liftFuncIr (appendStmtWithId id stmt)

            x -> error (show x)

