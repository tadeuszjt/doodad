{-# LANGUAGE FlexibleInstances #-}
module FuncIrUnused where

import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
--import Control.Monad.IO.Class

import IR hiding (withCurrentId)
import Monad
import ASTResolved


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


instance MonadFuncIR (DoM FuncIrUnusedState) where
    liftFuncIrState (StateT s) = do
        irFunc <- gets funcIr
        let (a, irFunc') = runIdentity (s irFunc)
        modify $ \s -> s { funcIr = irFunc' }
        return a


withCurrentId :: ID -> DoM FuncIrUnusedState a -> DoM FuncIrUnusedState a
withCurrentId id f = do
    oldId <- liftFuncIrState (gets irCurrentId)
    liftFuncIrState $ modify $ \s -> s { irCurrentId = id }
    a <- f
    liftFuncIrState $ modify $ \s -> s { irCurrentId = oldId }
    modify $ \s -> s { hasReturned = False }
    return a


funcIrUnused :: FuncIR -> DoM FuncIrUnusedState ()
funcIrUnused func = do
    liftFuncIrState $ modify $ \s -> s { irTypes = irTypes func }
    liftFuncIrState $ modify $ \s -> s { irIdSupply = irIdSupply func }

    forM_ (Map.toList $ irStmts func) $ \(id, stmt) -> case stmt of
        Block _ -> addUsed id
        Loop _  -> addUsed id
        If arg _ -> do
            addUsed id
            case arg of
                ArgID argId -> addUsed argId
                _           -> return ()

        Switch cases -> do
            addUsed id
            forM_ cases $ \(_, cnd, _) -> case cnd of
                ArgID argId -> addUsed argId
                _           -> return ()

        Break  -> addUsed id

        EmbedC strMap _ -> do
            mapM addUsed (map snd strMap)
            addUsed id

        SSA (InitVar marg) -> case marg of
            Nothing -> return ()
            Just (ArgConst _ _) -> return ()
            Just (ArgID argId) -> addUsed argId

        Return (ArgID argId) -> addUsed argId
        Return (ArgConst _ _) -> return ()

        SSA (MakeReferenceFromValue (ArgID i)) -> addUsed i
        SSA (MakeValueFromReference (ArgID i)) -> addUsed i

        SSA (MakeString str) -> return ()

        SSA (MakeSlice args) -> do
            forM_ args $ \arg -> case arg of
                ArgID i -> addUsed i
                x -> error (show x)


        SSA (Call _ args) -> do 
            forM_ args $ \arg -> case arg of
                ArgConst _ _ -> return ()
                ArgID argId  -> addUsed argId
            addUsed id

        SSA x -> error (show x)

    void $ processStmt func 0


processStmt :: FuncIR -> ID -> DoM FuncIrUnusedState ()
processStmt funcIr id = do
    hasReturned <- gets hasReturned
    unless hasReturned $ let stmt = irStmts funcIr Map.! id in case stmt of
        Block ids -> do
            unless (id == 0) $ appendStmtWithId id (Block [])
            withCurrentId id $ do
                mapM_ (processStmt funcIr) ids

        Switch cases -> do
            forM_ cases $ \(preBlkId, cnd, postBlkId) -> do
                addStmt preBlkId (Block [])
                addStmt postBlkId (Block [])

                let Just (Block preIds) = Map.lookup preBlkId (irStmts funcIr)
                let Just (Block postIds) = Map.lookup postBlkId (irStmts funcIr)

                withCurrentId preBlkId $ mapM_ (processStmt funcIr) preIds
                withCurrentId postBlkId $ mapM_ (processStmt funcIr) postIds

            appendStmtWithId id (Switch cases)



        Loop ids -> do
            void $ appendStmtWithId id (Loop [])
            withCurrentId id $ do
                mapM_ (processStmt funcIr) ids

        If (ArgConst _ (ConstBool True)) ids -> do
            mapM_ (processStmt funcIr) ids
            
        If (ArgConst _ (ConstBool False)) _ -> do
            return ()

        If cnd ids -> do
            appendStmtWithId id (If cnd [])
            withCurrentId id $ mapM_ (processStmt funcIr) ids

        Break -> do
            modify $ \s -> s { hasReturned = True }
            void $ appendStmtWithId id stmt

        EmbedC _ _ ->
            void $ appendStmtWithId id stmt

        Return arg -> do
            modify $ \s -> s { hasReturned = True }
            void $ appendStmtWithId id stmt

        SSA _ -> do
            used <- isUsed id
            when used $ void $ appendStmtWithId id stmt

        x -> error (show x)

