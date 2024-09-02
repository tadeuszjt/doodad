module FuncIrChecker where


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



data FuncIrCheckerState = FuncIrCheckerState
    { funcIr       :: FuncIR
    , astResolved  :: ASTResolved
    }


initFuncIrCheckerState ast = FuncIrCheckerState
    { funcIr      = initFuncIr
    , astResolved = ast
    }


liftFuncIr :: DoM FuncIR a -> DoM FuncIrCheckerState a
liftFuncIr f = do
    fn <- gets funcIr
    (a, fn') <- runDoMExcept fn f
    modify $ \s -> s { funcIr = fn' }
    return a


withCurrentId :: ID -> DoM FuncIrCheckerState a -> DoM FuncIrCheckerState a
withCurrentId id f = do
    oldId <- liftFuncIr (gets irCurrentId)
    liftFuncIr $ modify $ \s -> s { irCurrentId = id }
    a <- f
    liftFuncIr $ modify $ \s -> s { irCurrentId = oldId }
    return a


funcIrChecker :: FuncIR -> DoM FuncIrCheckerState FuncIR
funcIrChecker func = do
    processStmt func 0
    gets funcIr


getBaseArgOfRef :: FuncIR -> Arg -> DoM FuncIrCheckerState Arg
getBaseArgOfRef funcIr arg@(ArgConst _ _) = return arg
getBaseArgOfRef funcIr (ArgID id) = case Map.lookup id (irStmts funcIr) of
    Just (SSA op) -> case Map.lookup id (irTypes funcIr) of
        Just (_, Value) -> return (ArgID id)
        Just (_, Ref)   -> case op of
            MakeReferenceFromValue a -> return a
            Call _  args             -> do
                results <- fmap catMaybes $ forM args $ \arg -> case arg of
                    ArgID argId -> case Map.lookup argId (irTypes funcIr) of
                        Just (_, Ref) -> fmap Just $ getBaseArgOfRef funcIr (ArgID argId)
                        Just (_, Value) -> return Nothing

                        x -> error (show x)
                    x -> error (show x)

                case results of
                    [x] -> return x

    Nothing -> return (ArgID id) -- an arg
    x -> error (show x)
    


processStmt :: FuncIR -> ID -> DoM FuncIrCheckerState ()
processStmt funcIr id = let Just stmt = Map.lookup id (irStmts funcIr) in case stmt of
    EmbedC strMap _ -> return ()
    Block ids -> do
        withCurrentId id $ mapM_ (processStmt funcIr) ids

    Loop ids -> do
        withCurrentId id $ mapM_ (processStmt funcIr) ids

    If arg trueBlkId falseBlkId -> do
        let Just (Block trueIds) = Map.lookup trueBlkId (irStmts funcIr)
        let Just (Block falseIds) = Map.lookup falseBlkId (irStmts funcIr)
        withCurrentId trueBlkId $ mapM_ (processStmt funcIr) trueIds
        withCurrentId falseBlkId $ mapM_ (processStmt funcIr) falseIds

    Break        -> return ()
    ReturnVoid  -> return ()

    Return arg -> return ()
    SSA (InitVar ma) -> return ()

    SSA (Call callType args) -> do
        bases <- mapM (getBaseArgOfRef funcIr) args

        forM bases $ \arg -> do
            let num = length $ filter (== arg) bases
            case num of
                1 -> return ()
                _ -> fail ("multiple references to: " ++ show arg)
            

        return ()


    SSA (MakeReferenceFromValue (ArgID i)) -> return ()

    SSA (MakeValueFromReference _) -> return ()
    SSA (MakeString str)               -> return ()

    x -> error (show x)

