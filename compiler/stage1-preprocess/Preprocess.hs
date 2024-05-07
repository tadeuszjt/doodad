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

    _ -> return element
    

