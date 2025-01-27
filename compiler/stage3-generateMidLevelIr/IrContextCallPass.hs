{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IrContextCallPass where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Type
import Ir
import Error
import ASTResolved

import Debug.Trace


data IrContextCallState = IrContextCallState
    { funcIr :: FuncIr2
    , symTab :: [Map.Map Type Ir.ID]
    }


initIrContextCallState funcIr = IrContextCallState
    { funcIr = funcIr
    , symTab = [Map.empty]
    }


newtype IrContextCallPass a = IrContextCallPass
    { unIrContextCallPass :: StateT IrContextCallState (ReaderT ASTResolved (Except Error)) a }
    deriving (Functor, Applicative, Monad, MonadState IrContextCallState, MonadError Error,
        MonadReader ASTResolved)


runIrContextCallPass :: IrContextCallState -> ASTResolved -> IrContextCallPass a -> Either Error (a, IrContextCallState)
runIrContextCallPass irContextCallState astResolved f =
    runExcept $ runReaderT (runStateT (unIrContextCallPass f) irContextCallState) astResolved



define :: Type -> Ir.ID -> IrContextCallPass ()
define typ id = do
    resm <- gets (Map.lookup typ . head . symTab)
    unless (isNothing resm) (error $ "type already defined: " ++ show typ)
    modify $ \s -> s { symTab = (Map.insert typ id $ head $ symTab s) : tail (symTab s) }



look :: Type -> IrContextCallPass Ir.ID
look typ = do
    resm <- look' <$> gets symTab
    case resm of
        Nothing -> error "not defined"
        Just id -> return id
    where
        look' :: [Map.Map Type Ir.ID] -> Maybe Ir.ID
        look' (x : xs)
            | Map.member typ x = Just (x Map.! typ)
            | otherwise        = look' xs
        look' []               = Nothing


pushSymTab :: IrContextCallPass ()
pushSymTab = modify $ \s -> s { symTab = Map.empty : symTab s }


popSymTab :: IrContextCallPass ()
popSymTab = modify $ \s -> s { symTab = tail (symTab s) }


irContextCallPass :: State ASTResolved ()
irContextCallPass = do
    ast <- get

    xs <- forM (Set.toList $ instantiationsTop ast) $ \instType -> do
        let func = fromJust $ Map.lookup instType (instantiations ast)
        let resEither = runIrContextCallPass (initIrContextCallState func) ast irContextCallFunc
        case resEither of
            Right ((), x) -> return (instType, funcIr x)

    void $ forM xs $ \(instType, func) ->
        modify $ \s -> s { instantiations = Map.insert instType func (instantiations s) }



irContextCallFunc :: IrContextCallPass ()
irContextCallFunc = do
    contexts <- gets (fromJust . irContexts . funcIr)
    forM_ (Map.toList contexts) $ \(typ, id) -> define typ id
    irContextCallStmt 0
    

irContextCallStmt :: Ir.ID -> IrContextCallPass ()
irContextCallStmt stmtId = do
    stmt <- gets (fromJust . Map.lookup stmtId . irStmts . funcIr)
    case stmt of
        Block ids -> mapM_ irContextCallStmt ids
        Return _  -> return ()
        EmbedC _ _ -> return ()
        If arg ids -> mapM_ irContextCallStmt ids
        Loop ids -> mapM_ irContextCallStmt ids
        Break -> return ()
        MakeSlice _ _ -> return ()

        Call callType args -> do
            callIr <- fromJust . Map.lookup callType . instantiations <$> ask
            case Map.toList (fromJust $ irContexts callIr) of
                [] -> return ()

                xs -> do
                    ctxArgs <- forM xs $ \(t, _) -> ArgModify t <$> look t
                    modify $ \s -> s { funcIr = (funcIr s) { irStmts = Map.insert
                        stmtId (Call callType $ args ++ ctxArgs) (irStmts $ funcIr s)
                        }}

        With args ids -> do
            pushSymTab
            forM_ args $ \arg -> case arg of
                ArgModify typ id -> define typ id
            mapM_ irContextCallStmt ids
            popSymTab


        x -> error (show x)
