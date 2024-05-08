module Preprocess where

import Monad
import AST
import ASTMapper
import Symbol


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
            return $ ast { astStmts = stmts' }


preprocessMapper :: Elem -> DoM PreprocessState Elem
preprocessMapper element = case element of
    ElemExpr (AST.Int pos n) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Int pos n]
    ElemExpr (AST.Float pos f) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Float pos f]
    ElemExpr (AST.Bool pos b) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Bool pos b]
    ElemExpr (AST.Char pos c) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Char pos c]

    ElemStmt (Block stmts) -> return $ ElemStmt $ Scoped (Block stmts)

    ElemStmt (Let pos pattern mexpr (Just blk)) -> do
        return $ ElemStmt $ Scoped $ Block [Let pos pattern mexpr Nothing, blk]

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

