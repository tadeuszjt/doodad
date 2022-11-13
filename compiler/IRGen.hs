{-# LANGUAGE FlexibleContexts #-}
module IRGen where

import IR
import Monad
import qualified AST as S



data IRGenState
    = IRGenState
    {
    }


-- assumes flat AST
-- Top level comprised of typedefs, var defs and func defs with no circles
-- All symbols resolved into scopless names
-- All expressions annotated with inferred types
genIR :: BoM IRGenState m => S.AST -> m ()
genIR ast = do
    mapM_ genStmt (S.astStmts ast)


genStmt :: BoM IRGenState m => S.Stmt -> m ()
genStmt stmt = case stmt of
    _ -> fail (show stmt)
    
