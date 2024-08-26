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
import FindFunc
import qualified AST as S
import qualified MakeFuncIR as IR
import qualified FuncIrDestroy
import qualified FuncIrUnused as IR



data FuncIrInlineState = FuncIrInlineState
    { funcIr       :: FuncIR
    , astResolved  :: ASTResolved
    , idMap        :: Map.Map ID ID
    , returnId     :: ID 
    }


initFuncIrInlineState ast = FuncIrInlineState
    { funcIr      = initFuncIr
    , astResolved = ast
    , idMap       = Map.empty
    , returnId    = 0
    }


liftFuncIr :: DoM FuncIR a -> DoM FuncIrInlineState a
liftFuncIr f = do
    fn <- gets funcIr
    (a, fn') <- runDoMExcept fn f
    modify $ \s -> s { funcIr = fn' }
    return a


addIdMap :: ID -> ID -> DoM FuncIrInlineState ()
addIdMap idFrom idTo = do
    resm <- gets $ Map.lookup idFrom . idMap
    unless (isNothing resm) (error $ "id already mapped")
    modify $ \s -> s { idMap = Map.insert idFrom idTo (idMap s) }


getIdMap :: ID -> DoM FuncIrInlineState ID
getIdMap idFrom = do
    resm <- gets $ Map.lookup idFrom . idMap
    unless (isJust resm) (error $ "id not mapped: " ++ show idFrom)
    return (fromJust resm)


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


processStmt :: FuncIR -> ID -> DoM FuncIrInlineState ()
processStmt funcIr id = let Just stmt = Map.lookup id (irStmts funcIr) in case stmt of
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
        void $ liftFuncIr $ appendStmtWithId id stmt

    EmbedC _ _ ->
        void $ liftFuncIr (appendStmtWithId id stmt)

    Return arg -> do
        void $ liftFuncIr (appendStmtWithId id stmt)

    ReturnVoid -> do
        void $ liftFuncIr (appendStmtWithId id stmt)

    SSA typ Value (Call callType callArgs) -> do -- TODO can also inline ref calls
        ast <- gets astResolved
        callAst <- fmap fst $ runDoMExcept ast (makeInstance callType)
        callIr0  <- fmap (snd . fst) $ runDoMExcept (IR.initFuncIRState ast) (IR.makeFuncIR callAst)
        callIr1  <- fmap (FuncIrDestroy.funcIr . snd) $ runDoMExcept (FuncIrDestroy.initFuncIrDestroyState ast) (FuncIrDestroy.addFuncDestroy callIr0)
        callIr2  <- fmap (IR.funcIr . snd) $ runDoMExcept (IR.initFuncIrUnusedState ast) (IR.funcIrUnused callIr1)


        isInline <- fmap fst $ runDoMExcept () (functionIsInlineable callIr2)
        case isInline of
            False -> void $ liftFuncIr (appendStmtWithId id stmt)
            True  -> do
                modify $ \s -> s { idMap = Map.empty }
                forM_ (zip callArgs [1..]) $ \(callArg, from) -> case callArg of
                    ArgID i     -> addIdMap from i
                    ArgConst t _ -> do
                        i <- liftFuncIr $ appendSSA t Value (InitVar $ Just callArg)
                        addIdMap from i

                    x -> error (show x)

                arg' <- fmap last $ case Map.lookup 0 (irStmts callIr2) of
                    Just (Block ids) -> mapM (processInline callIr2) ids
                void $ liftFuncIr $ appendStmtWithId id (SSA typ Value $ MakeValueFromValue arg')


    SSA _ _ _ -> void $ liftFuncIr (appendStmtWithId id stmt)

    x -> error (show x)


-- this function can only be called on functions with a return on the last function exit
processInline :: FuncIR -> ID -> DoM FuncIrInlineState Arg
processInline callIr stmtId = do
    let Just stmt = Map.lookup stmtId (irStmts callIr)
    case stmt of
        Block ids -> do
            blkId <- liftFuncIr $ appendStmt (Block [])
            withCurrentId blkId $ do
                last <$> mapM (processInline callIr) ids

        If arg ids -> do
            arg' <- case arg of
                ArgID argId -> ArgID <$> getIdMap argId
                x -> error (show x)
            ifId <- liftFuncIr $ appendStmt (If arg' [])
            withCurrentId ifId $ do
                last <$> mapM (processInline callIr) ids

        EmbedC ids str -> do
            ids' <- mapM getIdMap ids
            void $ liftFuncIr $ appendStmt (EmbedC ids' str)
            return $ ArgID (-1)

        SSA typ refType op -> do
            op' <- processOp op
            id' <- liftFuncIr $ appendSSA typ refType op'
            addIdMap stmtId id'
            return (ArgID id')

        Return arg -> case arg of
            ArgConst typ const -> return arg
            ArgID argId -> ArgID <$> getIdMap argId
            x -> error (show x)
            
        x -> error (show x)


processOp :: Operation -> DoM FuncIrInlineState Operation
processOp operation = case operation of
    Call callType args -> fmap (Call callType) $ forM args $ \arg -> case arg of
        ArgID id -> ArgID <$> getIdMap id
        ArgConst t c -> return (ArgConst t c)
        x -> error (show x)

    InitVar Nothing -> return (InitVar Nothing)

    MakeReferenceFromValue id -> MakeReferenceFromValue <$> getIdMap id

    MakeValueFromReference id -> MakeValueFromReference <$> getIdMap id

    InitVar (Just arg) -> fmap (InitVar . Just) $ case arg of
        ArgID id -> ArgID <$> getIdMap id
        ArgConst t c -> return (ArgConst t c)
        x -> error (show x)

    

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
        (0, 1, Return _, x) -> return True
        (0, 0, _, x)        -> return True
        _                   -> return False


builtinStore :: Arg -> Arg -> DoM FuncIrInlineState ()
builtinStore dst src = do
    (t1, Ref) <- liftFuncIr $ getType dst
    (t2, refType) <- liftFuncIr $ getType src
    case refType of
        Value -> return ()
        Const -> return ()
    --unless (t1 == t2) (error "type mismatch")

    -- get store feature symbol
    ast <- gets astResolved
    let xs = Map.keys $ Map.filterWithKey
            (\k v -> symbolsCouldMatch k $ Sym ["store", "store"])
            (typeDefsAll ast)
    storeSymbol <- case xs of
        [] -> fail "builtin::builtinStore undefined"
        [x] -> return x


    acq <- fmap fst $ runDoMExcept ast $ makeAcquireInstance (foldType [TypeDef storeSymbol, t1])
    unless (isJust acq) (fail $ "no store acquire for: " ++ show t1)

    let acqSymbol = S.funcSymbol (fromJust acq)
    case S.funcArgs (fromJust acq) of
        [S.RefParam _ _ _, S.Param _ _ _] -> do
            void $ liftFuncIr $ appendSSA Void Const $ Call (Apply (TypeDef storeSymbol) t1) [dst, src]
