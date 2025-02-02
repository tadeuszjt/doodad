{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SemanticReferenceCheck where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State

import Ir
import Type
import Error
import ASTResolved


-- 1.) references are perishable if containing data structure is modified
-- 2.) ensure references are not used after possible perish
-- 3.) ensure args do not overlap with references


--data CheckReferenceState = CheckReferenceState
--    { activeReferences :: Map.Map ID [ID]
--    }
--
--
--initCheckReferenceState = CheckReferenceState
--    { activeReferences = Map.empty
--    }
--
--
--newtype CheckReference a = CheckReference
--    { unCheckReference :: StateT CheckReferenceState (Except Error) a }
--    deriving (Functor, Applicative, Monad, MonadState CheckReferenceState, MonadError Error)
--
--
--
--runCheckReference :: CheckReferenceState -> CheckReference a -> Except Error a
--runCheckReference checkReferenceState f =
--    fmap fst $ runStateT (unCheckReference f) checkReferenceState
--
--
--semanticReferenceCheck :: ASTResolved -> Except Error ()
--semanticReferenceCheck ast =
--    forM_ (instantiationsTop ast) $ \instType ->
--        runCheckReference initCheckReferenceState $ checkReferenceFuncIr $
--            fromJust $ Map.lookup instType (instantiations ast)
--
--
--checkReferenceFuncIr :: FuncIr -> CheckReference ()
--checkReferenceFuncIr funcIr = do
--    checkReferenceStmt funcIr 0
--    return ()
--
--
--checkReferenceStmt :: FuncIr -> ID -> CheckReference ()
--checkReferenceStmt funcIr id = case irStmts funcIr Map.! id of
--    Block ids -> mapM_ (checkReferenceStmt funcIr) ids
--    Return _ -> return ()
--    EmbedC _ _ -> return ()
--    Break -> return ()
--
--    If _ ids -> mapM_ (checkReferenceStmt funcIr) ids
--    Loop ids -> mapM_ (checkReferenceStmt funcIr) ids
--    With _ ids -> mapM_ (checkReferenceStmt funcIr) ids
--    MakeSlice _ _ -> return ()
--
--
--    Call retType atype _ args -> do
--        let arg = makeArg retType atype id
--        isRef <- argIsReference arg
--        case isRef of
--            False -> return ()
--            True  -> do
--                refIds <- fmap (map argId) $ filterM argIsReference args
--                modify $ \s -> s { activeReferences = Map.insert id refIds (activeReferences s) }
--
--
--    x -> error (show x)
--
--
--
--
--makeArg :: Type -> ArgType -> ID -> Arg
--makeArg t a i = case a of
--    ATModify -> ArgModify t i
--    ATValue  -> ArgValue  t i
--
--
--argIsReference :: Arg -> CheckReference Bool
--argIsReference arg = case arg of
--    x -> error (show x)
--    
