module FuncIrRunConst where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
--import Control.Monad.IO.Class
import Data.Maybe

import IR
import Monad
import ASTResolved
import Symbol
import Type
import FindFunc
import qualified AST as S

data FuncIrRunState = FuncIrUnusedState
    { funcIr       :: FuncIR
    , astResolved  :: ASTResolved
    , valueMap     :: Map.Map ID Type
    , currentBlock :: [ID]
    , currentIndex :: [Int]
    }


initFuncIrRunState ast = FuncIrUnusedState
    { funcIr      = initFuncIr
    , astResolved = ast
    , valueMap    = Map.empty
    , currentBlock = [0]
    , currentIndex = [0]
    }


liftFuncIr :: DoM FuncIR a -> DoM FuncIrRunState a
liftFuncIr f = do
    fn <- gets funcIr
    (a, fn') <- runDoMExcept fn f
    modify $ \s -> s { funcIr = fn' }
    return a



funcIrRun :: FuncIR -> DoM FuncIrRunState (Maybe Constant)
funcIrRun func = do
    modify $ \s -> s { funcIr = func }
    funcIrRun'
    where
        funcIrRun' :: DoM FuncIrRunState (Maybe Constant)
        funcIrRun' = do
            resm <- run
            modify $ \s -> s { currentIndex = (head (currentIndex s) + 1) : tail (currentIndex s) }
            case resm of
                Nothing -> funcIrRun'
                Just  x -> return (Just x)



addValue :: ID -> Type -> DoM FuncIrRunState ()
addValue id typ = do
    resm <- gets $ Map.lookup id . valueMap
    case resm of
        Just _ -> error $ "value already in map: " ++ show id
        Nothing -> return ()
    modify $ \s -> s { valueMap = Map.insert id (typ) (valueMap s) }



run :: DoM FuncIrRunState (Maybe Constant)
run = do
    currentBlk <- head <$> gets currentBlock
    currentIdx <- head <$> gets currentIndex

    blk <- liftFuncIr $ gets $ (Map.! currentBlk) . irStmts

    case blk of
        Block ids -> do
            if currentIdx >= (length ids) then do
                modify $ \s -> s { currentBlock = tail (currentBlock s) }
                modify $ \s -> s { currentIndex = tail (currentIndex s) }
                return Nothing
            else do
                let curStmtId = ids !! currentIdx
                curStmt <- liftFuncIr $ gets $ (Map.! curStmtId) . irStmts
                case curStmt of
                    Block ids -> do
                        modify $ \s -> s { currentBlock = curStmtId : (currentBlock s) }
                        modify $ \s -> s { currentIndex = 0 : (currentIndex s) }
                        return Nothing

                    EmbedC _ _ -> fail "cannot run embed c"

                    SSA typ Value (InitVar Nothing) -> do
                        addValue curStmtId typ
                        return Nothing
                        

                    x -> error (show x)




