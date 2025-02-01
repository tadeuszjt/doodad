{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SemanticReferenceCheck where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State

import Ir
import Error
import ASTResolved


-- 1.) references are perishable if containing data structure is modified
-- 2.) ensure references are not used after possible perish
-- 3.) ensure args do not overlap with references


data CheckReferenceState = CheckReferenceState
    { activeReferences :: Map.Map ID [ID]
    }


initCheckReferenceState = CheckReferenceState
    { activeReferences = Map.empty
    }


newtype CheckReference a = CheckReference
    { unCheckReference :: StateT CheckReferenceState (Except Error) a }
    deriving (Functor, Applicative, Monad, MonadState CheckReferenceState, MonadError Error)



runCheckReference :: CheckReferenceState -> CheckReference a -> Except Error a
runCheckReference checkReferenceState f =
    fmap fst $ runStateT (unCheckReference f) checkReferenceState


semanticReferenceCheck :: ASTResolved -> Except Error ()
semanticReferenceCheck ast =
    forM_ (instantiationsTop ast) $ \instType ->
        runCheckReference initCheckReferenceState $ checkReferenceFuncIr $
            fromJust $ Map.lookup instType (instantiations ast)


checkReferenceFuncIr :: FuncIr -> CheckReference ()
checkReferenceFuncIr funcIr = do
    checkReferenceStmt funcIr 0
    return ()


checkReferenceStmt :: FuncIr -> ID -> CheckReference ()
checkReferenceStmt funcIr id = case irStmts funcIr Map.! id of
    Block ids -> mapM_ (checkReferenceStmt funcIr) ids
    Return _ -> return ()
    EmbedC _ _ -> return ()
    Break -> return ()

    If _ ids -> mapM_ (checkReferenceStmt funcIr) ids
    Loop ids -> mapM_ (checkReferenceStmt funcIr) ids
    With _ ids -> mapM_ (checkReferenceStmt funcIr) ids
    MakeSlice _ _ -> return ()


    Call _ args -> case irIdArgs funcIr Map.! id of
        ArgValue _ _ -> return ()
        ArgModify _ _ -> do
            dependencies <- fmap catMaybes $ forM args $ \arg -> case arg of
                ArgModify _ i -> return (Just i)
                _             -> return Nothing
            modify $ \s -> s { activeReferences = Map.insert id dependencies (activeReferences s) }


        x -> error (show x)

    x -> error (show x)
    
