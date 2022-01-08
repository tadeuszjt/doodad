{-# LANGUAGE FlexibleContexts #-}
module Collect where

import AST
import Monad
import Error
import Control.Monad.State
import Type as T
import qualified SymTab


type SymTab = SymTab.SymTab Symbol SymKey Object

data SymKey
    = KeyVar
    deriving (Show, Eq)

data Object
    = ObjVar Type
    deriving (Show, Eq)


collectAST :: BoM SymTab m => AST -> m [(Type, Type)]
collectAST ast = do
    fmap concat $ mapM collectStmt (astStmts ast)


collectStmt :: BoM SymTab m => Stmt -> m [(Type, Type)]
collectStmt stmt = withPos stmt $ case stmt of
    FuncDef _ sym params retty blk -> do
        return []


    _ -> fail $ show stmt


collectExpr :: BoM SymTab m => Expr -> m [(Type, Type)]
collectExpr expr = case expr of
    _ -> fail "3213)21"
