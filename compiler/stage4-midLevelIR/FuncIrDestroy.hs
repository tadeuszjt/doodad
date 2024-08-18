module FuncIrDestroy where

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



data FuncIrDestroyState = FuncIrDestroyState
    { funcIr       :: FuncIR
    , idTable      :: Map.Map ID ID
    , astResolved  :: ASTResolved
    , destroyStack :: [Set.Set ID] -- a stack of 'Values' that have been created in this scope
    , destroySymbol :: Maybe Symbol
    }


initFuncIrDestroyState ast = FuncIrDestroyState
    { funcIr      = initFuncIr
    , idTable     = Map.empty
    , destroyStack = []
    , astResolved = ast
    , destroySymbol = Nothing
    }


liftFuncIr :: DoM FuncIR a -> DoM FuncIrDestroyState a
liftFuncIr f = do
    fn <- gets funcIr
    (a, fn') <- runDoMExcept fn f
    modify $ \s -> s { funcIr = fn' }
    return a


pushStack :: DoM FuncIrDestroyState ()
pushStack = do
    modify $ \s -> s { destroyStack = (Set.empty : (destroyStack s)) }

popStack :: DoM FuncIrDestroyState ()
popStack = do
    modify $ \s -> s { destroyStack = tail (destroyStack s) }

addDestroy :: ID -> DoM FuncIrDestroyState ()
addDestroy id = do
    set <- gets $ head . destroyStack
    when (Set.member id set) (fail "id already in destroy stack")
    modify $ \s -> s { destroyStack = (Set.insert id set) : (tail $ destroyStack s) }


addFuncDestroy :: FuncIR -> DoM FuncIrDestroyState ()
addFuncDestroy func = do
    -- get symbol for destroy::destroy(x)
    ast <- gets astResolved
    let xs = Map.keys $ Map.filterWithKey
            (\k v -> symbolsCouldMatch k $ Sym ["builtin", "destroy"])
            (typeDefsAll ast)
    case xs of
        [] -> return ()
        [x] -> modify $ \s -> s { destroySymbol = Just x }


    -- copy types from old map
    liftFuncIr $ modify $ \s -> s { irTypes = irTypes func }

    -- generate new ids from here
    liftFuncIr $ modify $ \s -> s { irIdSupply = irIdSupply func }

    void $ processStmt func 0


withCurrentId :: ID -> DoM FuncIrDestroyState a -> DoM FuncIrDestroyState a
withCurrentId id f = do
    oldId <- liftFuncIr (gets irCurrentId)
    liftFuncIr $ modify $ \s -> s { irCurrentId = id }
    a <- f
    liftFuncIr $ modify $ \s -> s { irCurrentId = oldId }
    return a


processStmt :: FuncIR -> ID -> DoM FuncIrDestroyState ()
processStmt funcIr id = let stmt = irStmts funcIr Map.! id in case stmt of
    Block ids -> do
        void $ liftFuncIr $ appendStmtWithId id (Block [])
        withCurrentId id $ do
            pushStack
            mapM_ (processStmt funcIr) ids

            -- destroy
            set <- Set.toList <$> gets (head . destroyStack)
            forM_ set $ \idToDestroy -> destroy idToDestroy
            popStack

    Loop ids -> do
        void $ liftFuncIr $ appendStmtWithId id (Loop [])
        withCurrentId id $ do
            pushStack
            mapM_ (processStmt funcIr) ids

            -- destroy
            set <- Set.toList <$> gets (head . destroyStack)
            --forM_ set $ \idToDestroy -> destroy idToDestroy
            popStack

    If cnd ids -> do
        void $ liftFuncIr $ appendStmtWithId id (If cnd [])
        withCurrentId id $ do
            pushStack
            mapM_ (processStmt funcIr) ids

            -- destroy
            set <- Set.toList <$> gets (head . destroyStack)
            --forM_ set $ \idToDestroy -> destroy idToDestroy
            popStack

    Else ids -> do
        void $ liftFuncIr $ appendStmtWithId id (Else [])
        withCurrentId id $ do
            pushStack
            mapM_ (processStmt funcIr) ids

            -- destroy
            set <- Set.toList <$> gets (head . destroyStack)
            --forM_ set $ \idToDestroy -> destroy idToDestroy
            popStack

    Break -> do
        -- destroy
        set <- Set.toList <$> gets (head . destroyStack)
        --forM_ set $ \idToDestroy -> destroy idToDestroy
        void $ liftFuncIr $ appendStmtWithId id stmt


    EmbedC _ _ ->
        void $ liftFuncIr (appendStmtWithId id stmt)

    Return _ -> do
        allSet <- gets $ concat . (map Set.toList) . destroyStack
        --forM_ allSet $ \idToDestroy -> destroy idToDestroy
        void $ liftFuncIr (appendStmtWithId id stmt)

    ReturnVoid -> do
        allSet <- gets $ concat . (map Set.toList) . destroyStack
        --forM_ allSet $ \idToDestroy -> destroy idToDestroy
        void $ liftFuncIr (appendStmtWithId id stmt)


    InitVar Nothing -> do
        addDestroy id
        (typ, refType) <- fmap fst $ runDoMExcept funcIr $ getType (ArgID id)
        void $ liftFuncIr (appendStmtWithId id stmt)

    InitVar _ -> do -- TODO this is not destroying because Preprocess using Assign to beat the system
        (typ, refType) <- fmap fst $ runDoMExcept funcIr $ getType (ArgID id)
        void $ liftFuncIr (appendStmtWithId id stmt)

    MakeReferenceFromValue _ -> do
        (typ, refType) <- fmap fst $ runDoMExcept funcIr $ getType (ArgID id)
        void $ liftFuncIr (appendStmtWithId id stmt)

    MakeValueFromReference _ -> do
        (typ, refType) <- fmap fst $ runDoMExcept funcIr $ getType (ArgID id)
        void $ liftFuncIr (appendStmtWithId id stmt)

    MakeFieldFromVal _ _ -> do
        (typ, refType) <- fmap fst $ runDoMExcept funcIr $ getType (ArgID id)
        void $ liftFuncIr (appendStmtWithId id stmt)

    MakeFieldFromRef _ _ -> do
        (typ, refType) <- fmap fst $ runDoMExcept funcIr $ getType (ArgID id)
        void $ liftFuncIr (appendStmtWithId id stmt)

    MakeString _ -> do
        (typ, refType) <- fmap fst $ runDoMExcept funcIr $ getType (ArgID id)
        void $ liftFuncIr (appendStmtWithId id stmt)

    Call _ _ -> do
        (typ, refType) <- fmap fst $ runDoMExcept funcIr $ getType (ArgID id)
        void $ liftFuncIr (appendStmtWithId id stmt)
        
    x -> error (show x)


destroy :: ID -> DoM FuncIrDestroyState ()
destroy id = do
    (typ, refType) <- liftFuncIr $ getType (ArgID id)

    destroySymbolm <- gets destroySymbol
    destroySymbol <- case destroySymbolm of
        Nothing -> fail "no destroy symbol" 
        Just  x -> return x

        
    ast <- gets astResolved
    acq <- fmap fst $ runDoMExcept ast $ makeAcquireInstance (foldType [TypeDef destroySymbol, typ])
    unless (isJust acq) (fail $ "no destroy for: " ++ show typ)

    let acqSymbol = S.funcSymbol $ S.funcHeader (fromJust acq)
    case S.funcArgs (S.funcHeader $ fromJust acq) of
        [S.RefParam _ argSymbol argType] -> do
            id1 <- liftFuncIr $ appendStmt (MakeReferenceFromValue id)
            liftFuncIr $ addType id1 typ Ref

            id2 <- liftFuncIr $ appendStmt $ Call (Apply (TypeDef destroySymbol) typ) [ArgID id1]
            liftFuncIr $ addType id2 Void Const


--addFuncDestroy :: ASTResolved -> DoM FuncIrDestroyState ()
--addFuncDestroy ast = do
--    -- need to find the symbol for destroy::destroy feature
--
--    let xs = Map.keys $ Map.filterWithKey
--            (\k v -> symbolsCouldMatch k $ Sym ["builtin", "destroy"])
--            (typeDefsAll ast)
--    mdestroySymbol <- case xs of
--        [] -> return Nothing
--        [x] -> return (Just x)
--    unless (isJust mdestroySymbol) (fail "no destroy symbol")
--    let destroySymbol = fromJust mdestroySymbol
--
--    statements <- gets (irStmts . funcIr)
--    statements' <- forM statements $ \statement -> case statement of
--        Block ids -> do
--            destroys <- getDestroys destroySymbol =<< getInits ids
--            return $ Block (ids ++ destroys)
--
--        EmbedC _ _ -> return statement
--        InitVar _ -> return statement
--        Return _ -> return statement
--        MakeReferenceFromValue _ -> return statement
--        Call _ _ -> return statement
--
--        x -> error (show x)
--
--    return ()
--    where
--        getDestroys :: Symbol -> [ID] -> DoM FuncIrDestroyState [ID]
--        getDestroys destroySymbol initIds = do
--            fmap concat $ forM initIds $ \id -> do
--                typ <- getType id
--                acq <- fmap fst $ runDoMExcept ast $
--                    makeAcquireInstance (foldType [TypeDef destroySymbol, typ])
--                unless (isJust acq) (fail $ "no destroy for: " ++ show typ)
--
--                let acqSymbol = S.funcSymbol $ S.funcHeader (fromJust acq)
--                case S.funcArgs (S.funcHeader $ fromJust acq) of
--                    [S.RefParam _ argSymbol argType] -> do
--                        unless (argType == typ) (error $ "argType was: " ++ show argType ++ " instead of: " ++ show typ)
--                        id1 <- createStmt Ref typ (MakeReferenceFromValue id)
--                        id2 <- createStmt Const Void (Call (Apply (TypeDef acqSymbol) typ) [ArgID id1])
--                        return [id1, id2]
--
--
--
--getInits :: [ID] -> DoM FuncIrDestroyState [ID]
--getInits []       = return []
--getInits (id:ids) = do
--    funcIr <- gets funcIr
--    case (irStmts funcIr) Map.! id of
--        InitVar argId -> (id:) <$> getInits ids
--        _             -> getInits ids
--    
--
--
