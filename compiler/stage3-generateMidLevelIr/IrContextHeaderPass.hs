{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Ir
import ASTResolved


type ContextStack = [Set.Set Type]


newtype IrContextHeader a = IrContextHeader
    { unIrContextHeader :: StateT ContextStack (StateT ASTResolved (Except Error)) a }
    deriving (Functor, Applicative, Monad, MonadState ContextStack, MonadError Error)


runIrContextHeader :: IrContextHeader a -> StateT ASTResolved (Except Error) (a, ContextStack)
runIrContextHeader f =
    runStateT (unIrContextHeader f) [Set.empty]


liftAstState :: StateT ASTResolved (Except Error) a -> IrContextHeader a
liftAstState = IrContextHeader . lift


irContextHeaderPass :: StateT ASTResolved (Except Error) ()
irContextHeaderPass = do
    top <- gets instantiationsTop

    contextsPrev <- forM (Set.toList top) $ \instType -> do
        funcIrm <- gets (Map.lookup instType . instantiations)
        case funcIrm of
            Just funcIr -> return (fmap Map.keys $ irContexts funcIr)

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

        ast <- get
        let resEither = runFuncIrMonad (instantiations ast Map.! instType) $ do
                ctxMap <- fmap Map.fromList $ forM (Set.toList set) $ \typ -> do
                    id <- generateId 
                    addStmt id $ Param (ArgRef typ Modify id)
                    return (typ, id)
                modify $ \s -> s { irContexts = Just ctxMap }
        funcIr' <- case resEither of
            Right ((), funcIr') -> return funcIr'
        modify $ \s -> s { instantiations = Map.insert instType funcIr' (instantiations s) }


    contextsNew <- forM (Set.toList top) $ \instType -> do
        funcIrm <- gets (Map.lookup instType . instantiations)
        case funcIrm of
            Just funcIr -> return (fmap Map.keys $ irContexts funcIr)

    unless (contextsPrev == contextsNew) irContextHeaderPass


pushStack :: Set.Set Type -> IrContextHeader ()
pushStack set = modify (set :)


popStack :: IrContextHeader ()
popStack = modify tail


addContext :: Type -> IrContextHeader ()
addContext typ = do
    stackMember <- gets $ any id . map (Set.member typ)
    unless stackMember $ modify (\s -> init s ++ [Set.insert typ $ last s])


irContextHeaderStmt :: FuncIr -> ID -> IrContextHeader ()
irContextHeaderStmt funcIr id = case irStmts funcIr Map.! id of
    Block ids  -> mapM_ (irContextHeaderStmt funcIr) ids
    Return _   -> return ()
    EmbedC _ _ -> return ()

    Call retArg callType _ -> do
        callIr <- liftAstState $ gets $ (Map.! callType) . instantiations
        case irContexts callIr of
            Just contexts -> forM_ (Map.keys contexts) $ addContext
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
