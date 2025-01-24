{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IrContextPass where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

import Error
import Type
import Symbol
import Ir2
import ASTResolved


data IrContextState = IrContextState
    { contextStack :: [Set.Set Type]
    , currentInst  :: Type
    , nothingFlag  :: Bool
    }


initIrContextState typ = IrContextState
    { contextStack = [Set.empty]
    , currentInst  = typ
    , nothingFlag  = False
    }


newtype IrContextPass a = IrContextPass
    { unIrContextPass :: StateT ASTResolved (Except Error) a }
    deriving (Functor, Applicative, Monad, MonadState ASTResolved, MonadError Error)


instance MonadFail IrContextPass where
    fail = throwError . ErrorStr


runIrContextPass :: ASTResolved -> IrContextPass a -> Either Error (a, ASTResolved)
runIrContextPass astResolved f =
    runExcept $ runStateT (unIrContextPass f) astResolved


newtype IrContextHeader a = IrContextHeader
    { unIrContextHeader :: StateT IrContextState (StateT ASTResolved (Except Error)) a }
    deriving (Functor, Applicative, Monad, MonadState IrContextState, MonadError Error)


instance MonadFail IrContextHeader where
    fail = throwError . ErrorStr


liftAstState :: State ASTResolved a -> IrContextHeader a
liftAstState (StateT s) = IrContextHeader $ lift $ StateT (pure . runIdentity . s)


runIrContextHeader :: IrContextState -> IrContextHeader a -> IrContextPass (a, IrContextState)
runIrContextHeader irContextState f =
    IrContextPass $ runStateT (unIrContextHeader f) irContextState


irContextPass :: IrContextPass ()
irContextPass = do
    irContextAddHeaderCtx

    --top <- gets instantiationsTop
    --forM_ (Set.toList top) $ \instType -> do
    --    Just funcIr <- gets (Map.lookup instType . instantiations)
    --    (_, ir') <- runDoMExcept funcIr . irContextAddInstructionCtx =<< get
    --    return ()


--irContextAddInstructionCtx :: ASTResolved -> DoM FuncIr2 ()
--irContextAddInstructionCtx ast = do
--    forM_ (Set.toList $ instantiationsTop ast) $ \instType -> do
--        error (show instType)


irContextAddHeaderCtx :: IrContextPass ()
irContextAddHeaderCtx = do
    top <- gets instantiationsTop

    contextsPrev <- forM (Set.toList top) $ \instType -> do
        Just funcIr <- gets (Map.lookup instType . instantiations)
        return (irContexts funcIr)

    -- TODO refactor
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


irContextInst :: Type -> IrContextPass (Set.Set Type)
irContextInst instType = do
    Just funcIr <- gets (Map.lookup instType . instantiations)
    ast <- get

    ((), state') <- runIrContextHeader (initIrContextState instType) (irContextStmt funcIr 0)
    let [set] = contextStack state'
    return set


addContexts :: Set.Set Type -> IrContextHeader ()
addContexts set = modify $ \s -> s
    { contextStack = (Set.union set $ head $ contextStack s ) : tail (contextStack s) }


irContextStmt :: FuncIr2 -> ID -> IrContextHeader ()
irContextStmt funcIr id = case irStmts funcIr Map.! id of
    Block ids  -> mapM_ (irContextStmt funcIr) ids
    Return _   -> return ()
    EmbedC _ _ -> return ()

    Call callType _ -> do
        curInst <- gets currentInst
        case callType == curInst of
            True -> return ()
            False -> do
                Just callIr <- liftAstState $ gets (Map.lookup callType . instantiations)
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


