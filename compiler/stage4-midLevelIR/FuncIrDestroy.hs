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
    , astResolved  :: ASTResolved
    , destroyStack :: [Set.Set ID] -- a stack of 'Values' that have been created in this scope
    }


initFuncIrDestroyState ast = FuncIrDestroyState
    { funcIr       = initFuncIr
    , destroyStack = []
    , astResolved  = ast
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
    -- copy types from old map
    liftFuncIr $ modify $ \s -> s { irTypes = irTypes func }

    -- generate new ids from here
    liftFuncIr $ modify $ \s -> s { irIdSupply = irIdSupply func }

    processStmt func 0


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
        unless (id == 0) $ void $ liftFuncIr $ appendStmtWithId id (Block [])
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
            forM_ set $ \idToDestroy -> destroy idToDestroy
            popStack

    If cnd trueBlkId falseBlkId -> do
        void $ liftFuncIr $ appendStmtWithId id (If cnd trueBlkId falseBlkId)
        liftFuncIr $ addStmt trueBlkId (Block [])
        liftFuncIr $ addStmt falseBlkId (Block [])

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
        -- destroy
        set <- Set.toList <$> gets (head . destroyStack)
        forM_ set $ \idToDestroy -> destroy idToDestroy
        void $ liftFuncIr $ appendStmtWithId id stmt


    EmbedC _ _ ->
        void $ liftFuncIr (appendStmtWithId id stmt)

    Return arg -> do
        allSet <- gets $ Set.unions . destroyStack
        allSet' <- case arg of -- don't destroy if returning stack var
            (ArgID argId) -> return (Set.delete argId allSet)
            (ArgConst _ _ ) -> return allSet
            x -> error (show x)

        mapM_ destroy allSet'
        void $ liftFuncIr (appendStmtWithId id stmt)

    ReturnVoid -> do
        allSet <- gets $ concat . (map Set.toList) . destroyStack
        forM_ allSet $ \idToDestroy -> destroy idToDestroy
        void $ liftFuncIr (appendStmtWithId id stmt)

    SSA _ _ (InitVar _) -> do
        addDestroy id
        void $ liftFuncIr (appendStmtWithId id stmt)

    SSA _ _ _ -> void $ liftFuncIr (appendStmtWithId id stmt)

    x -> error (show x)


destroy :: ID -> DoM FuncIrDestroyState ()
destroy id = do
    (typ, refType) <- liftFuncIr $ getType (ArgID id)

    -- get destroy symbol
    ast <- gets astResolved
    let xs = Map.keys $ Map.filterWithKey
            (\k v -> symbolsCouldMatch k $ Sym ["builtin", "destroy"])
            (typeDefsAll ast)
    destroySymbol <- case xs of
        [] -> fail "builtin::destroy undefined"
        [x] -> return x

    resm <- fmap fst $ runDoMExcept ast $ makeHeaderInstance (foldType [TypeDef destroySymbol, typ])
    [ParamIR Ref argType] <- case resm of
        Nothing -> fail ("no destroy instance for: " ++ show typ)
        Just irHeader -> return (irArgs irHeader)

    id1 <- liftFuncIr $ appendSSA typ Ref (MakeReferenceFromValue $ ArgID id)
    void $ liftFuncIr $ appendSSA Void Const $
        Call (Apply (TypeDef destroySymbol) typ) [ArgID id1]

