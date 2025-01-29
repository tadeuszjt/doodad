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
    { funcIr :: FuncIr
    , symTab :: [Map.Map Type Ir.ID]
    }


initIrContextCallState funcIr = IrContextCallState
    { funcIr = funcIr
    , symTab = [Map.empty]
    }


newtype IrContextCall a = IrContextCall
    { unIrContextCall :: StateT IrContextCallState (StateT ASTResolved (Except Error)) a }
    deriving (Functor, Applicative, Monad, MonadState IrContextCallState, MonadError Error)


liftAstState :: StateT ASTResolved (Except Error) a -> IrContextCall a
liftAstState m = IrContextCall (lift m)


runIrContextCall :: IrContextCallState -> ASTResolved -> IrContextCall a
    -> StateT ASTResolved (Except Error) (a, IrContextCallState)
runIrContextCall irContextCallState astResolved f =
    runStateT (unIrContextCall f) irContextCallState


irContextCallPass :: StateT ASTResolved (Except Error) ()
irContextCallPass = do
    ast <- get
    forM_ (Set.toList $ instantiationsTop ast) $ \instType -> do
        let func = fromJust $ Map.lookup instType (instantiations ast)
        state <- fmap snd $ runIrContextCall (initIrContextCallState func) ast irContextCallFunc
        modify $ \s -> s { instantiations = Map.insert instType (funcIr state) (instantiations s) }


define :: Type -> Ir.ID -> IrContextCall ()
define typ id = do
    resm <- gets (Map.lookup typ . head . symTab)
    unless (isNothing resm) (error $ "type already defined: " ++ show typ)
    modify $ \s -> s { symTab = (Map.insert typ id $ head $ symTab s) : tail (symTab s) }


look :: Type -> IrContextCall Ir.ID
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


pushSymTab :: IrContextCall ()
pushSymTab = modify $ \s -> s { symTab = Map.empty : symTab s }


popSymTab :: IrContextCall ()
popSymTab = modify $ \s -> s { symTab = tail (symTab s) }


irContextCallFunc :: IrContextCall ()
irContextCallFunc = do
    contexts <- gets (fromJust . irContexts . funcIr)
    forM_ (Map.toList contexts) $ \(typ, id) -> define typ id
    irContextCallStmt 0
    

irContextCallStmt :: Ir.ID -> IrContextCall ()
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
            callIr <- liftAstState $ gets (fromJust . Map.lookup callType . instantiations)
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
