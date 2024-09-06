{-# LANGUAGE FlexibleInstances #-}
module FuncIrConst where

import Control.Monad.Identity
import qualified Data.Set as Set
import qualified Data.Map as Map
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
import qualified MakeFuncIR as IR
import qualified FuncIrDestroy
import qualified FuncIrUnused as IR



data FuncIrConstState = FuncIrConstState
    { funcIr       :: FuncIR
    , astResolved  :: ASTResolved
    , constMap     :: Map.Map ID Arg
    }


initFuncIrConstState ast = FuncIrConstState
    { funcIr      = initFuncIr
    , astResolved = ast
    , constMap    = Map.empty
    }


instance MonadFuncIR (DoM FuncIrConstState) where
    liftFuncIrState (StateT s) = do
        irFunc <- gets funcIr
        let (a, irFunc') = runIdentity (s irFunc)
        modify $ \s -> s { funcIr = irFunc' }
        return a


addConst :: ID -> Arg -> DoM FuncIrConstState ()
addConst id arg = do
    --liftIO $ putStrLn $ "adding: " ++ show (id, arg)
    resm <- gets $ Map.lookup id . constMap
    unless (isNothing resm) (error "id already in constmap")
    modify $ \s -> s { constMap = Map.insert id arg (constMap s) }


getConst :: ID -> DoM FuncIrConstState Arg
getConst id = do
    resm <- gets $ Map.lookup id . constMap
    unless (isJust resm) (error "id not in constmap")
    return (fromJust resm)

removeConst :: ID -> DoM FuncIrConstState ()
removeConst id = do
    resm <- gets $ Map.lookup id . constMap
    --unless (isJust resm) (error "id not in constmap")
    modify $ \s -> s { constMap = Map.delete id (constMap s) }


funcIrConst :: FuncIR -> DoM FuncIrConstState FuncIR
funcIrConst func = do
    liftFuncIrState $ modify $ \s -> s { irTypes = irTypes func }
    liftFuncIrState $ modify $ \s -> s { irIdSupply = irIdSupply func }
    processStmt func 0
    gets funcIr


processArg :: Arg -> DoM FuncIrConstState Arg
processArg arg = case arg of
    ArgConst _ _ -> return arg
    ArgID argId -> do
        resm <- gets $ Map.lookup argId . constMap
        case resm of
            Just c  -> return c
            Nothing -> return arg

    x -> error (show x)


processStmt :: FuncIR -> ID -> DoM FuncIrConstState ()
processStmt funcIr id = let Just stmt = Map.lookup id (irStmts funcIr) in case stmt of
    Block ids -> do
        unless (id == 0) $ void $ appendStmtWithId id (Block [])
        withCurrentId id $ mapM_ (processStmt funcIr) ids

    Loop ids -> do
        modify $ \s -> s { constMap = Map.empty } -- TODO
        void $ appendStmtWithId id (Loop [])
        withCurrentId id $ mapM_ (processStmt funcIr) ids

    If arg trueBlkId falseBlkId -> do
        arg' <- processArg arg
        void $ appendStmtWithId id (If arg' trueBlkId falseBlkId)
        addStmt trueBlkId (Block [])
        addStmt falseBlkId (Block [])
        let Just (Block trueIds) = Map.lookup trueBlkId (irStmts funcIr)
        let Just (Block falseIds) = Map.lookup falseBlkId (irStmts funcIr)
        withCurrentId trueBlkId $ mapM_ (processStmt funcIr) trueIds
        withCurrentId falseBlkId $ mapM_ (processStmt funcIr) falseIds

    Break        -> void $ appendStmtWithId id stmt
    ReturnVoid  -> void $ appendStmtWithId id stmt

    EmbedC strMap _ -> do
        mapM_ removeConst (map snd strMap)
        void $ appendStmtWithId id stmt

    Return arg -> do
        arg' <- processArg arg
        void $ appendStmtWithId id $ Return arg'

    SSA (InitVar ma) -> do
        let Just (typ, Value) = Map.lookup id (irTypes funcIr)
        ma' <- case ma of
            Just (ArgConst _ _) -> addConst id (fromJust ma) >> return ma
            Just (arg)          -> do
                arg' <- processArg arg
                return (Just arg')

            Nothing -> case typ of 
                I64  -> do
                    addConst id $ ArgConst I64 (ConstInt 0)
                    return ma
                F32  -> do
                    addConst id $ ArgConst F32 (ConstFloat 0.0)
                    return ma
                F64  -> do
                    addConst id $ ArgConst F64 (ConstFloat 0.0)
                    return ma
                Bool -> do
                    addConst id $ ArgConst Bool (ConstBool False)
                    return ma
                _ -> return ma
                x -> error (show x)

        void $ appendStmtWithId id $ SSA (InitVar ma')

    SSA (Call callType args) -> do
        args' <- mapM processArg args
        void $ appendStmtWithId id (SSA $ Call callType args')

    SSA (MakeReferenceFromValue (ArgID i)) -> do
        removeConst i
        void $ appendStmtWithId id stmt

    SSA (MakeValueFromReference _) -> void $ appendStmtWithId id stmt
    SSA (MakeString str)           -> void $ appendStmtWithId id stmt
    SSA (MakeSlice args) -> void $ appendStmtWithId id . SSA . MakeSlice =<< mapM processArg args


    x -> error (show x)

