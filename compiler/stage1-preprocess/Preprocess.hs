module Preprocess where

import Control.Monad.State

import Monad
import AST
import ASTMapper
import AstBuilder
import Symbol
import Error


data PreprocessState = PreprocessState
    {
    }

initPreprocessState = PreprocessState


preprocess :: AST -> DoM s AST
preprocess ast = fmap fst $ runDoMExcept initPreprocessState preprocess'
    where
        preprocess' :: DoM PreprocessState AST
        preprocess' = do
            stmts' <- mapM (mapStmtM preprocessMapper) (astStmts ast)


--
--            (_, builderState) <- runDoMExcept initAstBuilderState (buildAst stmts')


            return $ ast { astStmts = stmts' }



buildAst :: [Stmt] -> DoM AstBuilderState ()
buildAst statements = do
    forM_ statements $ \statement -> withPos statement (buildStmt statement)



buildStmt :: Stmt -> DoM AstBuilderState ()
buildStmt statement = withPos statement $ case statement of
--    FuncDef (Func header stmt) -> do
--        id <- appendStmt $ FuncDef $ Func header (Block [])
--        void $ withCurId id (buildStmt stmt)
--
--    Block stmts -> do
--        id <- appendStmt (Block [])
--        withCurId id (mapM buildStmt stmts)
--
--    Scoped stmt -> do
--        id <- appendStmt (Block [])

    x -> error (show x)


preprocessMapper :: Elem -> DoM PreprocessState Elem
preprocessMapper element = case element of
    ElemExpr (AST.Int pos n) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Int pos n]
    ElemExpr (AST.Float pos f) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Float pos f]
    ElemExpr (AST.Bool pos b) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Bool pos b]
    ElemExpr (AST.Char pos c) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Char pos c]

    ElemStmt (Block stmts) -> return $ ElemStmt $ Scoped (Block stmts)

    ElemStmt (Let pos pattern mexpr stmts) -> do
        case stmts of
            [] -> return element
            _  -> return $ ElemStmt $ Scoped $ Block (Let pos pattern mexpr [] : stmts)

    ElemExpr (Subscript pos expr1 expr2) ->
        return $ ElemExpr $ Call pos (Sym ["At", "at"]) [Reference pos expr1, expr2]


    ElemStmt (If pos expr blk melse) -> do
        let expr' = Call pos (Sym ["Construct", "construct"]) [expr]
        return $ ElemStmt (If pos expr' blk melse)

    ElemStmt (While pos (Match _ _ _) blk) -> return element
    ElemStmt (While pos expr blk) -> do
        let expr' = Call pos (Sym ["Construct", "construct"]) [expr]
        return $ ElemStmt (While pos expr' blk)

    _ -> return element

