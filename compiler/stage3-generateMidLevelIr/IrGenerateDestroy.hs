{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IrGenerateDestroy where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import Ir
import Type
import Error
import Symbol
import ASTResolved
import IrGenerate hiding (liftAstState)


data DestroyState = DestroyState
    { destroyStack :: [[ID]]
    }


initDestroyState = DestroyState
    { destroyStack = [[]] 
    }


newtype IrGenerateDestroy a = IrGenerateDestroy
    { unIrGenerateDestroy :: StateT DestroyState (StateT FuncIr (StateT ASTResolved (Except Error))) a }
    deriving (Functor, Applicative, Monad, MonadState DestroyState, MonadError Error)


instance MonadFail IrGenerateDestroy where
    fail = throwError . ErrorStr


instance MonadFuncIr IrGenerateDestroy where
    liftFuncIrState (StateT s) = IrGenerateDestroy $ lift $ state (runIdentity . s)


instance MonadTypeDefs IrGenerateDestroy where
    getTypeDefs = liftAstState (gets typeDefsAll)


liftAstState :: StateT ASTResolved (Except Error) a -> IrGenerateDestroy a
liftAstState = IrGenerateDestroy . lift . lift


liftFuncIrMonad :: FuncIrMonad a -> IrGenerateDestroy a
liftFuncIrMonad f = do
    funcIr <- liftFuncIrState get
    case runExcept $ runStateT (unFuncIrMonad f) funcIr of
        Right (a, funcIr') -> liftFuncIrState (put funcIr') >> pure a
        Left err           -> error (show err)



runIrGenerateDestroy :: DestroyState -> FuncIr -> IrGenerateDestroy a
    -> StateT ASTResolved (Except Error) (a, FuncIr)
runIrGenerateDestroy destroyState funcIr f = do
    ((a, _), funcIr) <- runStateT (runStateT (unIrGenerateDestroy f) destroyState) funcIr
    return (a, funcIr)


-- add destroy for all top instances
irGenerateDestroyPass :: StateT ASTResolved (Except Error) ()
irGenerateDestroyPass = do
    mapM_ irGenerateDestroyInst =<< gets (Set.toList . instantiationsTop)


-- add destroy for specific instance
irGenerateDestroyInst :: Type -> StateT ASTResolved (Except Error) ()
irGenerateDestroyInst instType = do
    funcIr <- gets (fromJust . Map.lookup instType . instantiations)
    let (TypeDef symbol, _) = unfoldType instType
    unless (symbolsCouldMatch symbol $ Sym ["builtin", "store"]) $ do
        ((), funcIr') <- runIrGenerateDestroy initDestroyState initFuncIr (destroyIr funcIr)
        modify $ \s -> s { instantiations = Map.insert instType funcIr' (instantiations s) }
    

destroyIr :: FuncIr -> IrGenerateDestroy ()
destroyIr funcIr = do
    liftFuncIrState $ put $ funcIr
        { irStmts = Map.singleton 0 (Block [])
        , irCurrentId = 0
        }

    let Block ids = fromJust $ Map.lookup 0 (irStmts funcIr)
    mapM_ (destroyStmt funcIr) ids
    stmts <- liftFuncIrMonad (gets irStmts)
    let Block ids = stmts Map.! 0
    case ids of
        [] -> return ()
        xs -> case stmts Map.! (last xs) of
            Return _ -> return ()
            _        -> mapM_ destroy =<< destroyIdsAll


destroyStmt :: FuncIr -> Ir.ID -> IrGenerateDestroy ()
destroyStmt funcIr id = case fromJust $ Map.lookup id (irStmts funcIr) of
    Block ids -> do
        appendStmtWithId id (Block [])
        withCurrentId id $ mapM_ (destroyStmt funcIr) ids

    Return mid -> do
        case mid of
            Nothing -> mapM_ destroy =<< destroyIdsAll
            Just i  -> mapM_ destroy . filter (/=i) =<< destroyIdsAll
        void $ appendStmtWithId id (Return mid)

    EmbedC a b -> void $ appendStmtWithId id (EmbedC a b)

    Call a b -> do
        arg <- liftFuncIrMonad (getIdArg id)
        case arg of
            ArgValue typ i -> destroyAdd i
            _              -> return ()

        void $ appendStmtWithId id (Call a b)

    If arg ids -> do
        pushStack
        appendStmtWithId id (If arg [])
        withCurrentId id $ do
            mapM_ (destroyStmt funcIr) ids
            mapM_ destroy =<< destroyIdsHead
        popStack

    Loop ids -> do
        pushStack
        appendStmtWithId id (Loop [])
        withCurrentId id $ do
            mapM_ (destroyStmt funcIr) ids
            mapM_ destroy =<< destroyIdsHead
        popStack

    Break -> do
        mapM_ destroy =<< destroyIdsHead
        void $ appendStmtWithId id Break

    MakeSlice a b -> void $ appendStmtWithId id (MakeSlice a b)

    With args ids -> do
        appendStmtWithId id (With args [])
        withCurrentId id $ mapM_ (destroyStmt funcIr) ids
                
    x -> error (show x)


pushStack :: IrGenerateDestroy ()
pushStack = modify $ \s -> s { destroyStack = [] : destroyStack s }


popStack :: IrGenerateDestroy ()
popStack = modify $ \s -> s { destroyStack = tail (destroyStack s) }



destroyAdd :: Ir.ID -> IrGenerateDestroy ()
destroyAdd id =
    modify $ \s -> s { destroyStack = (id : (head $ destroyStack s)) : tail (destroyStack s) }


destroyIdsAll :: IrGenerateDestroy [ID]
destroyIdsAll = gets (concat . destroyStack)


destroyIdsHead :: IrGenerateDestroy [ID]
destroyIdsHead = gets (head . destroyStack)


destroy :: Ir.ID -> IrGenerateDestroy ()
destroy id = do
    symbol <- liftAstState (findSymbol $ Sym ["builtin", "destroy"])

    arg <- liftFuncIrMonad (getIdArg id)
    case arg of
        ArgValue typ _ | isNoDestroy typ -> return ()
        ArgValue typ i -> do
            callType' <- liftAstState $ do
                callType <- irGenerateInstance (foldType [TypeDef symbol, typ])
                irGenerateDestroyInst callType
                return callType

            callId <- appendStmt $ Call callType' [ArgModify typ i]
            liftFuncIrMonad $ addIdArg callId (ArgValue Tuple callId)


        x -> error (show x)

    where
        isNoDestroy :: Type -> Bool
        isNoDestroy typ = case unfoldType typ of
            (TypeDef symbol, _) -> symbolsCouldMatch symbol (Sym ["compare", "Ordering"])
            (Sum, xs)           -> all isNoDestroy xs
            (Tuple, [])         -> True
            (Slice, _)          -> True
            (Bool, _)           -> True
            (I64, _)            -> True
            _                   -> False
       
