module AstBuilder where

import qualified Data.Map as Map
import Control.Monad.State

import AST
import Monad


type ID = Int

globalId = 0

data AstBuilderState = AstBuilderState
    { statements :: Map.Map ID Stmt
    , idSupply   :: ID
    , curId      :: ID
    }


initAstBuilderState = AstBuilderState
    { statements = Map.singleton globalId (Block [])
    , idSupply   = globalId + 1
    , curId      = globalId
    }


generateId :: DoM AstBuilderState ID
generateId = do
    idSupply <- gets idSupply
    modify $ \s -> s { idSupply = idSupply + 1 }
    return idSupply



appendId :: ID -> DoM AstBuilderState ()
appendId id = do
    curId <- gets curId
    True <- gets (Map.member curId . statements)
    stmt <- gets $ (Map.! curId) . statements
    stmt' <- case stmt of
        Block ids -> return $ Block (ids ++ [Stmt id])
        x -> error (show x)

    modify $ \s -> s { statements = Map.insert curId stmt' (statements s) }


newStmt :: Stmt -> DoM AstBuilderState ID
newStmt stmt = do
    id <- generateId
    modify $ \s -> s { statements = Map.insert id stmt (statements s) }
    return id


appendStmt :: Stmt -> DoM AstBuilderState ID
appendStmt stmt = do
    id <- generateId
    appendId id
    modify $ \s -> s { statements = Map.insert id stmt (statements s) }
    return id


withCurId :: ID -> (DoM AstBuilderState a) -> DoM AstBuilderState a
withCurId id f = do
    prevId <- gets curId
    modify $ \s -> s { curId = id }
    a <- f
    modify $ \s -> s { curId = prevId }
    return a


