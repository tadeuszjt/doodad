{-# LANGUAGE FlexibleInstances #-}
module FuncIrChecker where

import Control.Monad.Identity
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe

import IR
import Monad
import ASTResolved



data FuncIrCheckerState = FuncIrCheckerState
    { funcIr       :: FuncIR
    , astResolved  :: ASTResolved
    }


initFuncIrCheckerState ast = FuncIrCheckerState
    { funcIr      = initFuncIr
    , astResolved = ast
    }


instance MonadFuncIR (DoM FuncIrCheckerState) where
    liftFuncIrState (StateT s) = do
        irFunc <- gets funcIr
        let (a, irFunc') = runIdentity (s irFunc)
        modify $ \s -> s { funcIr = irFunc' }
        return a


funcIrChecker :: FuncIR -> DoM FuncIrCheckerState ()
funcIrChecker func = do
    -- 1.) reference args cannot overlap memory
    -- 2.) Can only return references to ref args
    -- 3.) cannot use potentially invalid references
    void $ processStmt func 0


getRefSubject :: FuncIR -> Arg -> DoM FuncIrCheckerState (Maybe ID)
getRefSubject funcIr (ArgConst _ _) = return Nothing
getRefSubject funcIr (ArgID argId) = case Map.lookup argId (irTypes funcIr) of
    Just (_, Value)    -> return Nothing
    Just (_, IR.Ref)      -> getArgSubject funcIr (ArgID argId)
    Just (_, IR.Slice) -> getArgSubject funcIr (ArgID argId)


getArgSubject :: FuncIR -> Arg -> DoM FuncIrCheckerState (Maybe ID)
getArgSubject funcIr arg@(ArgConst _ _) = return Nothing
getArgSubject funcIr (ArgID id) = case Map.lookup id (irTypes funcIr) of
    Just (_, Value) -> return (Just id)
    Just (_, IR.Slice) -> case Map.lookup id (irStmts funcIr) of
        Nothing -> return (Just id) -- an arg
        Just (SSA (MakeSlice _ )) -> return (Just id)
        Just (SSA (MakeString _)) -> return (Just id)
        Just (SSA (Call _ args)) -> do
            results <- fmap catMaybes $ forM args $ \arg -> case arg of
                ArgConst _ _ -> return Nothing
                ArgID argId -> case Map.lookup argId (irTypes funcIr) of
                    Just (_, IR.Ref) -> fmap Just $ getArgSubject funcIr (ArgID argId)
                    Just (_, IR.Slice) -> fmap Just $ getArgSubject funcIr (ArgID argId)
                    Just (_, Value) -> return Nothing
            case results of
                []  -> return Nothing -- TODO fail ("what: " ++ show id)
                [x] -> return x

        x -> error (show x)

    Just (_, IR.Ref)   -> case Map.lookup id (irStmts funcIr) of
        Nothing                                       -> return (Just id) -- an arg
        Just (SSA (MakeReferenceFromValue (ArgID i))) -> return (Just i)
        Just (SSA (Call _ args)) -> do
            results <- fmap catMaybes $ forM args $ \arg -> case arg of
                ArgConst _ _ -> return Nothing
                ArgID argId -> case Map.lookup argId (irTypes funcIr) of
                    Just (_, IR.Ref) -> fmap Just $ getArgSubject funcIr (ArgID argId)
                    Just (_, IR.Slice) -> fmap Just $ getArgSubject funcIr (ArgID argId)
                    Just (_, Value) -> return Nothing

            case results of
                []  -> return Nothing -- TODO fail ("what: " ++ show id)
                [x] -> return x

    x -> error (show x)


processStmt :: FuncIR -> ID -> DoM FuncIrCheckerState ()
processStmt funcIr id = let Just stmt = Map.lookup id (irStmts funcIr) in case stmt of
    EmbedC strMap _ -> return ()
    Block ids ->  withCurrentId id $ mapM_ (processStmt funcIr) ids
    Loop ids ->  withCurrentId id $ mapM_ (processStmt funcIr) ids
    If arg ids -> withCurrentId id $ mapM_ (processStmt funcIr) ids
    Break        -> return ()

    Return arg -> do -- check is arg ref
        resm <- getRefSubject funcIr arg
        case resm of
            Nothing -> return ()
            Just argId -> case Map.lookup argId (irStmts funcIr) of
                Nothing -> case Map.lookup argId (irTypes funcIr) of
                    Just (_, IR.Ref) -> return ()
                    Just (_, IR.Slice) -> return ()
                    Just (_, Value) -> fail ("returning reference must be to reference argument")
                Just x -> fail ("cannot return reference to stack variable")

    SSA (InitVar ma) -> return ()
    SSA (MakeReferenceFromValue (ArgID i)) -> return ()
    SSA (MakeValueFromReference _)         -> return ()
    SSA (MakeString str)                   -> return ()

    SSA (MakeSlice args) -> return ()

    SSA (Call callType args) -> do
        bases <- fmap catMaybes $ mapM (getArgSubject funcIr) args

        forM bases $ \arg -> do
            let num = length $ filter (== arg) bases
            case num of
                1 -> return ()
                _ -> fail ("multiple references to: " ++ show arg)
            
        return ()

    x -> error (show x)

