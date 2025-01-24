module IrContextPass where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

import Type
import Symbol
import Ir2
import Monad
import ASTResolved


data IrContextState = IrContextState
    { astResolved  :: ASTResolved
    , contextStack :: [Set.Set Type]
    , currentInst  :: Type
    , nothingFlag  :: Bool
    }

initIrContextState ast typ = IrContextState
    { astResolved = ast
    , contextStack = [Set.empty]
    , currentInst  = typ
    , nothingFlag  = False
    }



addContexts :: Set.Set Type -> DoM IrContextState ()
addContexts set = modify $ \s -> s
    { contextStack = (Set.union set $ head $ contextStack s ) : tail (contextStack s) }


irContextPass :: DoM ASTResolved ()
irContextPass = do
    irContextAddHeaderCtx
    --irContextAddInstructionCtx


irContextAddInstructionCtx :: DoM ASTResolved ()
irContextAddInstructionCtx = do
    top <- gets instantiationsTop
    forM_ (Set.toList top) $ \instType -> do
        error (show instType)
        


irContextAddHeaderCtx :: DoM ASTResolved ()
irContextAddHeaderCtx = do
    top <- gets instantiationsTop

    contextsPrev <- forM (Set.toList top) $ \instType -> do
        Just funcIr <- gets (Map.lookup instType . instantiations)
        return (irContexts funcIr)

    forM_ (Set.toList top) $ \instType -> do
        set <- case isBuiltinContext instType of
            Just typ -> return (Set.singleton typ)
            Nothing  -> irContextInst instType
        modify $ \s -> s { instantiations = Map.insert
            instType
            (instantiations s Map.! instType) { irContexts = Just set }
            (instantiations s)
            }

    contextsNew <- forM (Set.toList top) $ \instType -> do
        Just funcIr <- gets (Map.lookup instType . instantiations)
        return (irContexts funcIr)

    unless (contextsPrev == contextsNew) irContextAddHeaderCtx


irContextInst :: Type -> DoM ASTResolved (Set.Set Type)
irContextInst instType = do
    Just funcIr <- gets (Map.lookup instType . instantiations)
    ast <- get

    (_, state') <- runDoMExcept (initIrContextState ast instType) (irContextStmt funcIr 0)
    let [set] = contextStack state'
    return set



irContextStmt :: FuncIr2 -> ID -> DoM IrContextState ()
irContextStmt funcIr id = case irStmts funcIr Map.! id of
    Block ids  -> mapM_ (irContextStmt funcIr) ids
    Return _   -> return ()
    EmbedC _ _ -> return ()


    Call callType _ -> do
        curInst <- gets currentInst
        case callType == curInst of
            True -> return ()
            False -> do
                Just callIr <- gets (Map.lookup callType . instantiations . astResolved)
                case irContexts callIr of
                    Just contexts -> addContexts contexts
                    Nothing       -> do
                        modify $ \s -> s { nothingFlag = True }

    If _ ids -> mapM_ (irContextStmt funcIr) ids
    Loop ids -> mapM_ (irContextStmt funcIr) ids
    Break    -> return ()
    MakeSlice _ _ -> return ()

    x -> error (show x)


isBuiltinContext :: Type -> Maybe Type
isBuiltinContext typ = let (TypeDef symbol, ts) = unfoldType typ in
    case symbolsCouldMatch symbol (Sym ["builtin", "builtinContext"]) of
        False -> Nothing
        True -> case ts of
            [t] -> Just t


