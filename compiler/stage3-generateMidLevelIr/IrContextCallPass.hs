{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IrContextCallPass where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Ir
import Error
import ASTResolved


data IrContextCallState = IrContextCallState
    { funcIr :: FuncIr2
    }


initIrContextCallState funcIr = IrContextCallState
    { funcIr = funcIr
    }


newtype IrContextCallPass a = IrContextCallPass
    { unIrContextCallPass :: StateT IrContextCallState (ReaderT ASTResolved (Except Error)) a }
    deriving (Functor, Applicative, Monad, MonadState IrContextCallState, MonadError Error,
        MonadReader ASTResolved)



runIrContextCallPass :: IrContextCallState -> ASTResolved -> IrContextCallPass a -> Either Error (a, IrContextCallState)
runIrContextCallPass irContextCallState astResolved f =
    runExcept $ runReaderT (runStateT (unIrContextCallPass f) irContextCallState) astResolved



irContextCallPass :: State ASTResolved ()
irContextCallPass = do
    ast <- get

    forM_ (Set.toList $ instantiationsTop ast) $ \instType -> do
        let func = fromJust $ Map.lookup instType (instantiations ast)
        let resEither = runIrContextCallPass (initIrContextCallState func) ast irContextCallFunc
        case resEither of
            Right ((), x) -> modify $ \s -> s {
                instantiations = Map.insert instType (funcIr x) (instantiations ast)
                }



irContextCallFunc :: IrContextCallPass ()
irContextCallFunc = do
    irContextCallStmt 0
    

irContextCallStmt :: Ir.ID -> IrContextCallPass ()
irContextCallStmt id = do
    stmt <- gets (fromJust . Map.lookup id . irStmts . funcIr)
    case stmt of
        Block ids -> mapM_ irContextCallStmt ids
        Return _  -> return ()
        Call callType args -> do
            callIr <- fromJust . Map.lookup callType . instantiations <$> ask
            case Set.toList (fromJust $ irContexts callIr) of
                [] -> return ()


        EmbedC _ _ -> return ()
        If arg ids -> do
            mapM_ irContextCallStmt ids
        Loop ids -> mapM_ irContextCallStmt ids
        Break -> return ()
        MakeSlice _ _ -> return ()

        x -> error (show x)




