module Preprocess where

import Monad
import AST
import ASTMapper
import Symbol
import AstBuilder
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


            ((), builderState) <- runDoMExcept initAstBuilderState (mapM_ buildStmt stmts')

            return $ ast { astStmts = stmts' }


buildStmt :: Stmt -> DoM AstBuilderState ()
buildStmt statement = withPos statement $ case statement of
    FuncDef (Func header (Block stmts)) -> do
        blockId <- newStmt (Block [])
        funcId <- appendStmt $ FuncDef (Func header (Stmt blockId))
        withCurId blockId $
            mapM_ buildStmt stmts


    _ -> return ()
    x -> error (show x)


preprocessMapper :: Elem -> DoM PreprocessState Elem
preprocessMapper element = case element of
    ElemExpr (AST.Int pos n) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Int pos n]
    ElemExpr (AST.Float pos f) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Float pos f]
    ElemExpr (AST.Bool pos b) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Bool pos b]
    ElemExpr (AST.Char pos c) -> return $ ElemExpr $ Call pos (Sym ["Construct", "construct"]) [AST.Char pos c]

    ElemStmt (Let pos pattern mexpr (Just blk)) -> do
        return $ ElemStmt $ Block [Let pos pattern mexpr Nothing, blk]

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

