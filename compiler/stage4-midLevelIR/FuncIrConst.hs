module FuncIrConst where

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


liftFuncIr :: DoM FuncIR a -> DoM FuncIrConstState a
liftFuncIr f = do
    fn <- gets funcIr
    (a, fn') <- runDoMExcept fn f
    modify $ \s -> s { funcIr = fn' }
    return a


withCurrentId :: ID -> DoM FuncIrConstState a -> DoM FuncIrConstState a
withCurrentId id f = do
    oldId <- liftFuncIr (gets irCurrentId)
    liftFuncIr $ modify $ \s -> s { irCurrentId = id }
    a <- f
    liftFuncIr $ modify $ \s -> s { irCurrentId = oldId }
    return a


funcIrConst :: FuncIR -> DoM FuncIrConstState FuncIR
funcIrConst func = do
    liftFuncIr $ modify $ \s -> s { irTypes = irTypes func }
    liftFuncIr $ modify $ \s -> s { irIdSupply = irIdSupply func }
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
        unless (id == 0) $ void $ liftFuncIr $ appendStmtWithId id (Block [])
        withCurrentId id $ mapM_ (processStmt funcIr) ids

    Loop ids -> do
        modify $ \s -> s { constMap = Map.empty } -- TODO
        void $ liftFuncIr $ appendStmtWithId id (Loop [])
        withCurrentId id $ mapM_ (processStmt funcIr) ids

    If arg trueBlkId falseBlkId -> do
        arg' <- processArg arg
        void $ liftFuncIr $ appendStmtWithId id (If arg' trueBlkId falseBlkId)
        liftFuncIr $ addStmt trueBlkId (Block [])
        liftFuncIr $ addStmt falseBlkId (Block [])
        let Just (Block trueIds) = Map.lookup trueBlkId (irStmts funcIr)
        let Just (Block falseIds) = Map.lookup falseBlkId (irStmts funcIr)
        withCurrentId trueBlkId $ mapM_ (processStmt funcIr) trueIds
        withCurrentId falseBlkId $ mapM_ (processStmt funcIr) falseIds

    Break        -> void $ liftFuncIr $ appendStmtWithId id stmt
    ReturnVoid  -> void $ liftFuncIr $ appendStmtWithId id stmt

    EmbedC strMap _ -> do
        mapM_ removeConst (map snd strMap)
        void $ liftFuncIr (appendStmtWithId id stmt)

    Return arg -> do
        arg' <- processArg arg
        void $ liftFuncIr (appendStmtWithId id $ Return arg')

    SSA typ Value (InitVar ma) -> do
        case ma of
            Just (ArgConst _ _) -> addConst id (fromJust ma)
            Nothing -> case typ of 
                I64  -> addConst id $ ArgConst I64 (ConstInt 0)
                F32  -> addConst id $ ArgConst F32 (ConstFloat 0.0)
                F64  -> addConst id $ ArgConst F64 (ConstFloat 0.0)
                Bool -> addConst id $ ArgConst Bool (ConstBool False)
                _ -> return ()
                x -> error (show x)

        void $ liftFuncIr (appendStmtWithId id stmt)

    SSA typ refType (Call callType args) -> do
        args' <- mapM processArg args
        void $ liftFuncIr $ appendStmtWithId id (SSA typ refType $ Call callType args')

    SSA _ Ref (MakeReferenceFromValue (ArgID i)) -> do
        removeConst i
        void $ liftFuncIr $ appendStmtWithId id stmt

    SSA _ Value (MakeValueFromReference _) -> void $ liftFuncIr $ appendStmtWithId id stmt
    SSA _ _ (MakeString str)               -> void $ liftFuncIr $ appendStmtWithId id stmt

    x -> error (show x)

