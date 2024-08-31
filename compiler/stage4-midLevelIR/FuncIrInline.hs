module FuncIrInline where

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
import Error
import FindFunc
import qualified AST as S
import qualified MakeFuncIR as IR
import qualified FuncIrDestroy
import qualified FuncIrUnused as IR



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


liftFuncIr :: DoM FuncIR a -> DoM FuncIrInlineState a
liftFuncIr f = do
    fn <- gets funcIr
    (a, fn') <- runDoMExcept fn f
    modify $ \s -> s { funcIr = fn' }
    return a


addIdMap :: ID -> Arg -> DoM FuncIrInlineState ()
addIdMap idFrom argTo = do
    resm <- gets $ Map.lookup idFrom . idMap
    unless (isNothing resm) (error $ "id already mapped: " ++ show (idFrom, argTo))
    modify $ \s -> s { idMap = Map.insert idFrom argTo (idMap s) }


withCurrentId :: ID -> DoM FuncIrInlineState a -> DoM FuncIrInlineState a
withCurrentId id f = do
    oldId <- liftFuncIr (gets irCurrentId)
    liftFuncIr $ modify $ \s -> s { irCurrentId = id }
    a <- f
    liftFuncIr $ modify $ \s -> s { irCurrentId = oldId }
    return a


funcIrInline :: FuncIR -> DoM FuncIrInlineState FuncIR
funcIrInline func = do
    liftFuncIr $ modify $ \s -> s { irTypes = irTypes func }
    liftFuncIr $ modify $ \s -> s { irIdSupply = irIdSupply func }
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
        unless (id == 0) $ void $ liftFuncIr $ appendStmtWithId id (Block [])
        withCurrentId id $ mapM_ (processStmt funcIr) ids

    EmbedC strMap str -> do
        strMap' <- forM strMap $ \(s, id) -> do
            ArgID id' <- processArg (ArgID id)
            return (s, id')
        void $ liftFuncIr $ appendStmtWithId id (EmbedC strMap' str)


    Loop ids -> do
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


    Break      -> void $ liftFuncIr $ appendStmtWithId id stmt
    Return arg -> void $ liftFuncIr . appendStmtWithId id . Return =<< processArg arg
    ReturnVoid -> void $ liftFuncIr (appendStmtWithId id stmt)

    SSA typ callRefType (Call callType callArgs) -> do -- TODO can also inline ref calls
        callArgs' <- mapM processArg callArgs

        --ast <- gets astResolved
--        Just callAst <- fmap fst $ runDoMExcept ast (makeInstance callType)
--        callIr0  <- fmap (snd . fst) $ runDoMExcept (IR.initFuncIRState ast) (IR.makeFuncIR callAst)
--        callIr1  <- fmap (FuncIrDestroy.funcIr . snd) $ runDoMExcept (FuncIrDestroy.initFuncIrDestroyState ast) (FuncIrDestroy.addFuncDestroy callIr0)
--        callIr2  <- fmap (IR.funcIr . snd) $ runDoMExcept (IR.initFuncIrUnusedState ast) (IR.funcIrUnused callIr1)

        resm <- gets $ Map.lookup callType . funcInstance . astResolved
        case resm of
            Nothing -> void $ liftFuncIr $ appendStmtWithId id $ SSA typ callRefType (Call callType callArgs')
            Just (_, callIr2) -> do
                isInline <- fmap fst $ runDoMExcept () (functionIsInlineable callIr2)
                case isInline of
                    False -> void $ liftFuncIr $ appendStmtWithId id $ SSA typ callRefType (Call callType callArgs')
                    True  -> do
                        --liftIO $ putStrLn $ "inlining: " ++ show callType
                        -- give the inline processor an idMap from inline to local
                        oldMap <- gets idMap
                        modify $ \s -> s { idMap = Map.empty }
                        zipWithM_ addIdMap [1..] callArgs'
                        retArg <- processInline callIr2 0
                        modify $ \s -> s { idMap = oldMap }
                        case typ of
                            Void -> return ()
                            _    -> addIdMap id retArg


    SSA typ refType (MakeReferenceFromValue arg) -> do
        arg'@(ArgID _) <- processArg arg
        void $ liftFuncIr $ appendStmtWithId id $ SSA typ refType (MakeReferenceFromValue arg')

    SSA typ refType (MakeValueFromReference arg) -> do
        arg'@(ArgID _) <- processArg arg
        void $ liftFuncIr $ appendStmtWithId id $ SSA typ refType (MakeValueFromReference arg')

    SSA typ refType (InitVar marg) -> do
        marg' <- traverse processArg marg
        void $ liftFuncIr $ appendStmtWithId id $ SSA typ refType (InitVar marg')

    SSA typ refType (MakeString str) -> do
        void $ liftFuncIr $ appendStmtWithId id $ SSA typ refType (MakeString str)

    x -> error (show x)


-- this function can only be called on functions with a return on the last function exit
processInline :: FuncIR -> ID -> DoM FuncIrInlineState Arg
processInline callIr stmtId = let Just stmt = Map.lookup stmtId (irStmts callIr) in case stmt of
    Block ids | stmtId == 0 -> do
        last <$> mapM (processInline callIr) ids
    Block ids -> do
        blkId <- liftFuncIr $ appendStmt (Block [])
        withCurrentId blkId $ last <$> mapM (processInline callIr) ids

    If arg trueBlkId falseBlkId -> do
        arg' <- processArg arg

        trueBlkId' <- liftFuncIr $ generateId
        falseBlkId' <- liftFuncIr $ generateId
        liftFuncIr $ appendStmt (If arg' trueBlkId' falseBlkId')
        liftFuncIr $ addStmt trueBlkId' (Block [])
        liftFuncIr $ addStmt falseBlkId' (Block [])

        let Just (Block trueIds) = Map.lookup trueBlkId (irStmts callIr)
        let Just (Block falseIds) = Map.lookup falseBlkId (irStmts callIr)
        withCurrentId trueBlkId' $ mapM_ (processInline callIr) trueIds
        withCurrentId falseBlkId' $ mapM_ (processInline callIr) falseIds
        return undefined

    EmbedC idMap str -> do
        ids' <- mapM (\(ArgID x) -> return x) =<< mapM (processArg . ArgID) (map snd idMap)
        void $ liftFuncIr $ appendStmt $ EmbedC (zip (map fst idMap) ids') str
        return undefined

    Return arg -> do
        case arg of
            ArgID 1 -> case Map.lookup 1 (irTypes callIr) of
                Just (typ, Value) -> do
                    arg' <- processArg arg
                    fmap ArgID $ liftFuncIr $ appendSSA typ Value $ (InitVar $ Just arg')
            _ -> processArg arg

    ReturnVoid -> return undefined

    SSA typ refType (Call callType args) -> do
        args' <- mapM processArg args
        addIdMap stmtId . ArgID =<< liftFuncIr (appendSSA typ refType $ (Call callType args'))
        return undefined

    SSA typ refType (InitVar marg) -> do
        marg' <- traverse processArg marg
        addIdMap stmtId . ArgID =<< liftFuncIr (appendSSA typ refType $ (InitVar marg'))
        return undefined

    SSA typ refType (MakeReferenceFromValue arg) -> do
        arg' <- processArg arg
        addIdMap stmtId . ArgID =<< liftFuncIr (appendSSA typ refType $ (MakeReferenceFromValue arg'))
        return undefined
        
    SSA typ refType (MakeValueFromReference arg) -> do
        arg' <- processArg arg
        addIdMap stmtId . ArgID =<< liftFuncIr (appendSSA typ refType $ (MakeValueFromReference arg'))
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
