{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module IrContextHeaderPass where

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


type ContextStack = [Set.Set Type]


newtype IrContextPass a = IrContextPass
    { unIrContextPass :: StateT ASTResolved (Except Error) a }
    deriving (Functor, Applicative, Monad, MonadState ASTResolved, MonadError Error)



newtype IrContextHeader a = IrContextHeader
    { unIrContextHeader :: StateT ContextStack (StateT ASTResolved (Except Error)) a }
    deriving (Functor, Applicative, Monad, MonadState ContextStack, MonadError Error)


instance MonadFail IrContextHeader where
    fail = throwError . ErrorStr


runIrContextPass :: ASTResolved -> IrContextPass a -> Either Error (a, ASTResolved)
runIrContextPass astResolved f =
    runExcept $ runStateT (unIrContextPass f) astResolved


runIrContextHeader :: IrContextHeader a -> IrContextPass (a, ContextStack)
runIrContextHeader f =
    IrContextPass $ runStateT (unIrContextHeader f) [Set.empty]



liftAstStateHeader :: State ASTResolved a -> IrContextHeader a
liftAstStateHeader (StateT s) = IrContextHeader $ lift $ StateT (pure . runIdentity . s)


pushStack :: MonadState ContextStack m => Set.Set Type -> m ()
pushStack set = modify (set :)


popStack :: MonadState ContextStack m => m ()
popStack = modify tail


addContext :: MonadState ContextStack m => Type -> m ()
addContext typ = do
    stackMember <- gets $ any id . map (Set.member typ)
    unless stackMember $ modify (\s -> init s ++ [Set.insert typ $ last s])


irContextHeaderPass :: IrContextPass ()
irContextHeaderPass = do
    top <- gets instantiationsTop

    contextsPrev <- forM (Set.toList top) $ \instType -> do
        funcIrm <- gets (Map.lookup instType . instantiations)
        case funcIrm of
            Just funcIr -> return (irContexts funcIr)

    -- TODO refactor
    forM_ (Set.toList top) $ \instType -> do
        set <- case isBuiltinContext instType of
            Just typ -> return (Set.singleton typ)
            Nothing  -> do
                funcIrm <- gets (Map.lookup instType . instantiations)
                funcIr <- case funcIrm of
                    Just funcIr -> return funcIr

                sets <- fmap snd $ runIrContextHeader (irContextHeaderStmt funcIr 0)
                case sets of
                    [set] -> return set

        modify $ \s -> s { instantiations = Map.insert
            instType
            (instantiations s Map.! instType) { irContexts = Just set }
            (instantiations s)
            }

    contextsNew <- forM (Set.toList top) $ \instType -> do
        funcIrm <- gets (Map.lookup instType . instantiations)
        case funcIrm of
            Just funcIr -> return (irContexts funcIr)

    unless (contextsPrev == contextsNew) irContextHeaderPass


irContextHeaderStmt :: FuncIr2 -> ID -> IrContextHeader ()
irContextHeaderStmt funcIr id = case irStmts funcIr Map.! id of
    Block ids  -> mapM_ (irContextHeaderStmt funcIr) ids
    Return _   -> return ()
    EmbedC _ _ -> return ()

    Call callType _ -> do
        Just callIr <- liftAstStateHeader $ gets (Map.lookup callType . instantiations)
        case irContexts callIr of
            Just contexts -> forM_ (Set.toList contexts) $ addContext
            Nothing       -> return ()

    With args ids -> do
        pushStack (Set.fromList $ map typeof args)
        mapM_ (irContextHeaderStmt funcIr) ids
        popStack

    If _ ids -> mapM_ (irContextHeaderStmt funcIr) ids
    Loop ids -> mapM_ (irContextHeaderStmt funcIr) ids
    Break    -> return ()
    MakeSlice _ _ -> return ()

    x -> error (show x)


isBuiltinContext :: Type -> Maybe Type
isBuiltinContext typ = let (TypeDef symbol, ts) = unfoldType typ in
    case symbolsCouldMatch symbol (Sym ["builtin", "builtinContext"]) of
        False -> Nothing
        True -> case ts of
            [t] -> Just t
