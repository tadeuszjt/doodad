{-# LANGUAGE FlexibleInstances #-}
module FuncIrInline where

import Control.Monad.Identity
import qualified Data.Map as Map
import Control.Monad.State
--import Control.Monad.IO.Class
import Data.Maybe

import IR
import Monad
import ASTResolved



data FuncIrInlineState = FuncIrInlineState
    { funcIr       :: FuncIR
    , astResolved  :: ASTResolved
    , idMap        :: Map.Map ID Arg
    }


initFuncIrInlineState ast = FuncIrInlineState
    { funcIr      = initFuncIr
    , astResolved = ast
    , idMap       = Map.empty
    }


instance MonadFuncIR (DoM FuncIrInlineState) where
    liftFuncIrState (StateT s) = do
        irFunc <- gets funcIr
        let (a, irFunc') = runIdentity (s irFunc)
        modify $ \s -> s { funcIr = irFunc' }
        return a


addIdMap :: ID -> Arg -> DoM FuncIrInlineState ()
addIdMap idFrom argTo = do
    resm <- gets $ Map.lookup idFrom . idMap
    unless (isNothing resm) (error $ "id already mapped: " ++ show (idFrom, argTo))
    modify $ \s -> s { idMap = Map.insert idFrom argTo (idMap s) }


funcIrInline :: FuncIR -> DoM FuncIrInlineState FuncIR
funcIrInline func = do
    liftFuncIrState $ modify $ \s -> s { irTypes = irTypes func }
    liftFuncIrState $ modify $ \s -> s { irIdSupply = irIdSupply func }
    processStmt func 0
    gets funcIr


processArg :: Arg -> DoM FuncIrInlineState Arg
processArg arg = case arg of
    ArgConst _ _ -> return arg
    ArgID argId -> do
        resm <- gets $ Map.lookup argId . idMap
        case resm of
            Nothing -> return (ArgID argId)
            Just arg' -> return arg'

    x -> error (show x)


processStmt :: FuncIR -> ID -> DoM FuncIrInlineState ()
processStmt funcIr id = let Just stmt = Map.lookup id (irStmts funcIr) in case stmt of
    Block ids -> do
        unless (id == 0) $ void $ appendStmtWithId id (Block [])
        withCurrentId id $ mapM_ (processStmt funcIr) ids

    EmbedC strMap str -> do
        strMap' <- forM strMap $ \(s, id) -> do
            ArgID id' <- processArg (ArgID id)
            return (s, id')
        void $ appendStmtWithId id (EmbedC strMap' str)


    Loop ids -> do
        void $ appendStmtWithId id (Loop [])
        withCurrentId id $ mapM_ (processStmt funcIr) ids

    If arg ids -> do
        arg' <- processArg arg
        void $ appendStmtWithId id (If arg' [])
        withCurrentId id $ mapM_ (processStmt funcIr) ids

    Break      -> void $ appendStmtWithId id stmt
    Return arg -> void $ appendStmtWithId id . Return =<< processArg arg

    SSA (Call callType callArgs) -> do -- TODO can also inline ref calls
        callArgs' <- mapM processArg callArgs

        --ast <- gets astResolved
--        Just callAst <- fmap fst $ runDoMExcept ast (makeInstance callType)
--        callIr0  <- fmap (snd . fst) $ runDoMExcept (IR.initFuncIRState ast) (IR.makeFuncIR callAst)
--        callIr1  <- fmap (FuncIrDestroy.funcIr . snd) $ runDoMExcept (FuncIrDestroy.initFuncIrDestroyState ast) (FuncIrDestroy.addFuncDestroy callIr0)
--        callIr2  <- fmap (IR.funcIr . snd) $ runDoMExcept (IR.initFuncIrUnusedState ast) (IR.funcIrUnused callIr1)

        resm <- gets $ Map.lookup callType . funcInstance . astResolved
        case resm of
            Nothing -> void $ appendStmtWithId id $ SSA (Call callType callArgs')
            Just (_, callIr2) -> do
                isInline <- fmap fst $ runDoMExcept () (functionIsInlineable callIr2)
                case isInline of
                    False -> void $ appendStmtWithId id $ SSA (Call callType callArgs')
                    True  -> do
                        --liftIO $ putStrLn $ "inlining: " ++ show callType
                        -- give the inline processor an idMap from inline to local
                        oldMap <- gets idMap
                        modify $ \s -> s { idMap = Map.empty }
                        zipWithM_ addIdMap [1..] callArgs'
                        retArg <- processInline callIr2 0
                        modify $ \s -> s { idMap = oldMap }
                        addIdMap id retArg


    SSA (MakeReferenceFromValue arg) -> do
        arg'@(ArgID _) <- processArg arg
        void $ appendStmtWithId id $ SSA (MakeReferenceFromValue arg')

    SSA (MakeValueFromReference arg) -> do
        arg'@(ArgID _) <- processArg arg
        void $ appendStmtWithId id $ SSA (MakeValueFromReference arg')

    SSA (InitVar marg) -> do
        marg' <- traverse processArg marg
        void $ appendStmtWithId id $ SSA (InitVar marg')

    SSA (MakeString str) -> do
        void $ appendStmtWithId id $ SSA (MakeString str)

    SSA (MakeSlice args) -> do
        void $ appendStmtWithId id . SSA . MakeSlice =<< mapM processArg args

    x -> error (show x)


-- this function can only be called on functions with a return on the last function exit
processInline :: FuncIR -> ID -> DoM FuncIrInlineState Arg
processInline callIr stmtId = let Just stmt = Map.lookup stmtId (irStmts callIr) in case stmt of
    Block ids | stmtId == 0 -> do
        last <$> mapM (processInline callIr) ids
    Block ids -> do
        blkId <- appendStmt (Block [])
        withCurrentId blkId $ last <$> mapM (processInline callIr) ids

    If arg ids -> do
        arg' <- processArg arg
        ifId <- appendStmt (If arg' [])
        withCurrentId ifId $ mapM_ (processInline callIr) ids
        return undefined

    EmbedC idMap str -> do
        ids' <- mapM (\(ArgID x) -> return x) =<< mapM (processArg . ArgID) (map snd idMap)
        void $ appendStmt $ EmbedC (zip (map fst idMap) ids') str
        return undefined

    Return arg -> do
        case arg of
            ArgID 1 -> case Map.lookup 1 (irTypes callIr) of
                Just (typ, Value) -> do
                    arg' <- processArg arg
                    fmap ArgID $ appendSSA typ Value $ (InitVar $ Just arg')
            _ -> processArg arg

    SSA (Call callType args) -> do
        let Just (typ, refType) = Map.lookup stmtId (irTypes callIr)
        args' <- mapM processArg args
        addIdMap stmtId . ArgID =<< appendSSA typ refType (Call callType args')
        return undefined

    SSA (InitVar marg) -> do
        let Just (typ, refType) = Map.lookup stmtId (irTypes callIr)
        marg' <- traverse processArg marg
        addIdMap stmtId . ArgID =<< appendSSA typ refType (InitVar marg')
        return undefined

    SSA (MakeReferenceFromValue arg) -> do
        let Just (typ, refType) = Map.lookup stmtId (irTypes callIr)
        arg' <- processArg arg
        addIdMap stmtId . ArgID =<< appendSSA typ refType (MakeReferenceFromValue arg')
        return undefined
        
    SSA (MakeValueFromReference arg) -> do
        let Just (typ, refType) = Map.lookup stmtId (irTypes callIr)
        arg' <- processArg arg
        addIdMap stmtId . ArgID =<< appendSSA typ refType (MakeValueFromReference arg')
        return undefined

    x -> error (show x)


functionIsInlineable :: FuncIR -> DoM () Bool
functionIsInlineable funcIr = do
    returnCount <- fmap (foldl (+) 0) $ forM (Map.toList $ irStmts funcIr) $ \(id, stmt) -> case stmt of
        Return _ -> return 1
        _        -> return 0

    embedCount <- fmap (foldl (+) 0) $ forM (Map.toList $ irStmts funcIr) $ \(id, stmt) -> case stmt of
        EmbedC _ _ -> return 1
        _          -> return 0

    let stmtCount = Map.size (irStmts funcIr)
    let Just (Block ids) = Map.lookup 0 (irStmts funcIr)
    let Just lastStmt = Map.lookup (last ids) (irStmts funcIr)

    case (embedCount, returnCount, lastStmt, stmtCount) of
        (0, 1, Return _, x) | stmtCount < 10 -> return True
        (0, 0, _, x)        | stmtCount < 10 -> return True
        _                                   -> return False
