{-# LANGUAGE FlexibleInstances #-}
module FuncIrDestroy where

import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
--import Control.Monad.IO.Class

import IR
import Monad
import ASTResolved
import Symbol
import Type
import FindFunc
import Error



data FuncIrDestroyState = FuncIrDestroyState
    { funcIr       :: FuncIR
    , astResolved  :: ASTResolved
    , destroyStack :: [Set.Set ID] -- a stack of 'Values' that have been created in this scope
    }


initFuncIrDestroyState ast = FuncIrDestroyState
    { funcIr       = initFuncIr
    , destroyStack = []
    , astResolved  = ast
    }

instance MonadFuncIR (DoM FuncIrDestroyState) where
    liftFuncIrState (StateT s) = do
        irFunc <- gets funcIr
        let (a, irFunc') = runIdentity (s irFunc)
        modify $ \s -> s { funcIr = irFunc' }
        return a


addFuncDestroy :: ASTResolved -> FuncIR -> DoM a FuncIR
addFuncDestroy ast func = do
    fmap (funcIr . snd) $ runDoMExcept state (processStmt func 0)
    where
        state = (initFuncIrDestroyState ast) { funcIr = initFuncIr
            { irTypes = irTypes func
            , irIdSupply = irIdSupply func
            , irTextPos = irTextPos func
            }}


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


processStmt :: FuncIR -> ID -> DoM FuncIrDestroyState ()
processStmt funcIr id = let stmt = irStmts funcIr Map.! id in case stmt of
    Block ids -> do
        unless (id == 0) $ void $ appendStmtWithId id (Block [])
        withCurrentId id $ do
            pushStack
            mapM_ (processStmt funcIr) ids

            -- destroy
            set <- Set.toList <$> gets (head . destroyStack)
            forM_ set $ \idToDestroy -> destroy idToDestroy
            popStack

    Loop ids -> do
        void $ appendStmtWithId id (Loop [])
        withCurrentId id $ do
            pushStack
            mapM_ (processStmt funcIr) ids

            -- destroy
            set <- Set.toList <$> gets (head . destroyStack)
            forM_ set $ \idToDestroy -> destroy idToDestroy
            popStack

    If cnd trueBlkId falseBlkId -> do
        void $ appendStmtWithId id (If cnd trueBlkId falseBlkId)
        addStmt trueBlkId (Block [])
        addStmt falseBlkId (Block [])

        withCurrentId trueBlkId $ do
            let Block ids = irStmts funcIr Map.! trueBlkId
            pushStack
            mapM_ (processStmt funcIr) ids
            -- destroy
            set <- Set.toList <$> gets (head . destroyStack)
            forM_ set $ \idToDestroy -> destroy idToDestroy
            popStack

        withCurrentId falseBlkId $ do
            let Block ids = irStmts funcIr Map.! falseBlkId
            pushStack
            mapM_ (processStmt funcIr) ids
            -- destroy
            set <- Set.toList <$> gets (head . destroyStack)
            forM_ set $ \idToDestroy -> destroy idToDestroy
            popStack


    Break -> do
        --TODO Bug - need to clean from all block levels created in last loop
        set <- Set.toList <$> gets (head . destroyStack)
        forM_ set $ \idToDestroy -> destroy idToDestroy
        void $ appendStmtWithId id stmt


    EmbedC _ _ ->
        void $ appendStmtWithId id stmt

    Return arg -> do
        allSet <- gets $ Set.unions . destroyStack
        allSet' <- case arg of -- don't destroy if returning stack var
            (ArgID argId) -> return (Set.delete argId allSet)
            (ArgConst _ _ ) -> return allSet
            x -> error (show x)

        mapM_ destroy allSet'
        void $ appendStmtWithId id stmt

    SSA (InitVar _) -> do
        addDestroy id
        void $ appendStmtWithId id stmt

    SSA _ -> void $ appendStmtWithId id stmt

    x -> error (show x)


destroy :: ID -> DoM FuncIrDestroyState ()
destroy id = do
    (typ, refType) <- getType (ArgID id)

    -- get destroy symbol
    ast <- gets astResolved
    let xs = Map.keys $ Map.filterWithKey
            (\k v -> symbolsCouldMatch k $ Sym ["builtin", "destroy"])
            (typeDefsAll ast)
    destroySymbol <- case xs of
        [] -> fail "builtin::destroy undefined"
        [x] -> return x

    resm <- fmap fst $ runDoMExcept ast $ makeHeaderInstance (foldType [TypeDef destroySymbol, typ])
    -- TODO get textPos here
    [ParamIR IR.Ref argType] <- case resm of
        Nothing -> fail ("no destroy instance for: " ++ show typ)
        Just irHeader -> return (irArgs irHeader)

    id1 <- appendSSA typ IR.Ref (MakeReferenceFromValue $ ArgID id)
    void $ appendSSA Tuple Value $
        Call (Apply (TypeDef destroySymbol) typ) [ArgID id1]

