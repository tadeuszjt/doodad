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
    , usedSet      :: Set.Set ID
    }


initFuncIrUnusedState ast = FuncIrUnusedState
    { funcIr      = initFuncIr
    , astResolved = ast
    , hasReturned = False
    , usedSet     = Set.empty
    }


addUsed :: ID -> DoM FuncIrUnusedState ()
addUsed id = do
    modify $ \s -> s { usedSet = Set.insert id (usedSet s) }

isUsed :: ID -> DoM FuncIrUnusedState Bool
isUsed id = do
    gets $ Set.member id . usedSet


liftFuncIr :: DoM FuncIR a -> DoM FuncIrUnusedState a
liftFuncIr f = do
    fn <- gets funcIr
    (a, fn') <- runDoMExcept fn f
    modify $ \s -> s { funcIr = fn' }
    return a



funcIrUnused :: FuncIR -> DoM FuncIrUnusedState ()
funcIrUnused func = do
    liftFuncIr $ modify $ \s -> s { irTypes = irTypes func }
    liftFuncIr $ modify $ \s -> s { irIdSupply = irIdSupply func }

    forM_ (Map.toList $ irStmts func) $ \(id, stmt) -> case stmt of
        Block _ -> addUsed id
        Loop _  -> addUsed id
        If arg _ _ -> do
            addUsed id
            case arg of
                ArgID argId -> addUsed argId
                _           -> return ()

        Break  -> addUsed id

        EmbedC strMap _ -> do
            mapM addUsed (map snd strMap)
            addUsed id

        SSA typ Value (InitVar marg) -> case marg of
            Nothing -> return ()
            Just (ArgConst _ _) -> return ()
            Just (ArgID argId) -> addUsed argId

        Return (ArgID argId) -> addUsed argId
        Return (ArgConst _ _) -> return ()
        ReturnVoid            -> return ()

        SSA typ Ref (MakeReferenceFromValue (ArgID i)) -> addUsed i
        SSA typ Value (MakeValueFromReference (ArgID i)) -> addUsed i

        SSA _ _ (MakeString str) -> return ()

        SSA _ _ (Call _ args) -> do 
            forM_ args $ \arg -> case arg of
                ArgConst _ _ -> return ()
                ArgID argId  -> addUsed argId
            addUsed id

        SSA _ _ x -> error (show x)

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
    unless hasReturned $ let stmt = irStmts funcIr Map.! id in case stmt of
        Block ids -> do
            unless (id == 0) $ liftFuncIr $ appendStmtWithId id (Block [])
            withCurrentId id $ do
                mapM_ (processStmt funcIr) ids

        Loop ids -> do
            void $ liftFuncIr $ appendStmtWithId id (Loop [])
            withCurrentId id $ do
                mapM_ (processStmt funcIr) ids

        If (ArgConst _ (ConstBool True)) trueBlkId falseBlkId -> do
            let Just (Block trueIds) = Map.lookup trueBlkId (irStmts funcIr)
            mapM_ (processStmt funcIr) trueIds
            
        If (ArgConst _ (ConstBool False)) trueBlkId falseBlkId -> do
            let Just (Block falseIds) = Map.lookup falseBlkId (irStmts funcIr)
            mapM_ (processStmt funcIr) falseIds

        If cnd trueBlkId falseBlkId -> do
            void $ liftFuncIr $ appendStmtWithId id (If cnd trueBlkId falseBlkId)
            liftFuncIr $ addStmt trueBlkId (Block [])
            liftFuncIr $ addStmt falseBlkId (Block [])

            let Just (Block trueIds) = Map.lookup trueBlkId (irStmts funcIr)
            let Just (Block falseIds) = Map.lookup falseBlkId (irStmts funcIr)
            withCurrentId trueBlkId $ mapM_ (processStmt funcIr) trueIds
            withCurrentId falseBlkId $ mapM_ (processStmt funcIr) falseIds

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

        SSA _ _ _ -> do
            used <- isUsed id
            when used $ void $ liftFuncIr (appendStmtWithId id stmt)

        x -> error (show x)

