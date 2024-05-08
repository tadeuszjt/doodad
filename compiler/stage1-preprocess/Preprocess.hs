{-# LANGUAGE FlexibleInstances #-}
module Preprocess where

import Control.Monad.Identity
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import Monad
import AST
import ASTMapper
import Symbol
import AstBuilder
import Error


preprocess :: AST -> DoM s AST
preprocess ast = fmap fst $ runDoMExcept () preprocess'
    where
        preprocess' :: DoM s AST
        preprocess' = do
            stmts' <- mapM (mapStmtM preprocessMapper) (astStmts ast)


            ((), buildState) <- runDoMExcept initBuildState (mapM_ buildStmt stmts')
            stmts'' <- fmap fst $ runDoMExcept (astBuilderState buildState) unpackAst

            return $ ast { astStmts = stmts'' }


data BuildState = BuildState 
    { astBuilderState :: AstBuilderState
    , supply          :: Int
    }


initBuildState = BuildState
    { astBuilderState = initAstBuilderState
    , supply = 0
    }


fresh :: DoM BuildState Int
fresh = do
    n <- gets supply
    modify $ \s -> s { supply = n + 1 }
    return n


freshSym :: DoM BuildState Symbol
freshSym = do
    id <- fresh
    return (Sym ["_x" ++ show id])


instance MonadAstBuilder (DoM BuildState) where
    liftAstBuilderState (StateT s) = DoM $
        StateT (\a -> pure $ (\(x, b) -> (x, a { astBuilderState = b })) $ runIdentity $ s $ astBuilderState a)


unpackAst :: DoM AstBuilderState [Stmt]
unpackAst = do
    Block topStmts <- gets $ (Map.! globalId) . statements
    mapM unpackStmt topStmts


unpackStmt :: Stmt -> DoM AstBuilderState Stmt
unpackStmt statement = withPos statement $ case statement of
    EmbedC _ _ -> return statement
    Return _ _  -> return statement
    ExprStmt _ -> return statement
    Data _ _ _ _ -> return statement
    Feature _ _ _ -> return statement

    Stmt id -> unpackStmt =<< gets ((Map.! id) . statements)

    FuncDef (Func header stmt) -> FuncDef . Func header <$> unpackStmt stmt
    Block stmts -> Block <$> mapM unpackStmt stmts
    Let pos pat mexpr mblk -> Let pos pat mexpr <$> traverse unpackStmt mblk
    Typedef pos generics symbol typ -> return statement
    For pos expr mpat stmt -> For pos expr mpat <$> unpackStmt stmt
    While pos expr stmt    -> While pos expr <$> unpackStmt stmt
    If pos expr blk mblk -> do
        blk' <- unpackStmt blk
        mblk' <- traverse unpackStmt mblk
        return (If pos expr blk' mblk')
    Switch pos expr cases -> do
        fmap (Switch pos expr) $ forM cases $ \(pat, stmt) -> do
            stmt' <- unpackStmt stmt
            return (pat, stmt')

    x -> error (show x)


buildPattern :: Pattern -> Expr -> DoM BuildState Expr
buildPattern pattern expr = do
    liftIO $ putStrLn "here"

    withPos pattern $ case pattern of
        PatIdent pos symbol -> do
            liftIO $ putStrLn "here"
            appendStmt $ Let pos (PatIdent pos symbol) Nothing Nothing
            appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"]) [Reference pos (Ident pos symbol), expr]
            return (AST.Bool pos True)

        PatLiteral literal -> do
            return $ Call (textPos pattern) (Sym ["Compare", "equal"]) [literal, expr]

        PatAnnotated pat patTyp -> case pat of
            PatLiteral literal -> do
                return $ Call (textPos pattern) (Sym ["Compare", "equal"]) [AExpr patTyp literal, expr]

            PatIdent pos symbol -> do
                appendStmt $ Let pos (PatIdent pos symbol) Nothing Nothing
                appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"])
                    [ Reference pos (AExpr patTyp $ Ident pos symbol)
                    , expr
                    ]
                return (AST.Bool pos True)

            x -> error (show x)

        PatTuple pos pats -> do
            symbol <- freshSym
            appendStmt $ Let pos (PatIdent pos symbol) Nothing Nothing
            appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"]) [Reference pos (Ident pos symbol), expr]

            let syms = ["first", "second", "third", "fourth"]
            boolExprs <- forM (zip pats [0..]) $ \(pat, i) -> do
                buildPattern pat $ Call pos (Sym [syms !! i]) [Reference pos $ Ident pos symbol]

            return (head boolExprs)

                
            

        x -> error (show x)


buildStmt :: Stmt -> DoM BuildState ()
buildStmt statement = withPos statement $ case statement of
    ExprStmt expr    -> void $ appendStmt (ExprStmt expr)
    Typedef _ _ _ _  -> void $ appendStmt statement
    EmbedC _ _       -> void $ appendStmt statement
    Return pos mexpr -> void $ appendStmt statement
    Data _ _ _ _     -> void $ appendStmt statement
    Feature _ _ _    -> void $ appendStmt statement

    FuncDef (Func header (Block stmts)) -> do
        blockId <- newStmt (Block [])
        funcId <- appendStmt $ FuncDef (Func header (Stmt blockId))
        withCurId blockId (mapM_ buildStmt stmts)

    Let pos pat mexpr mblk -> do
        case mblk of
            Nothing -> do
                void $ appendStmt (Let pos pat mexpr Nothing)
--                case mexpr of
--                    Just expr -> do
--                        boolExpr <- buildPattern pat expr
--                        void $ appendStmt $ ExprStmt $ Call pos (Sym ["assert"]) [boolExpr]
--
--                    Nothing -> do
--                        void $ appendStmt (Let pos pat mexpr Nothing)

    If pos expr blk mblk -> do
        blkId <- newStmt (Block [])
        withCurId blkId (buildStmt blk)
        mblkIdm <- case mblk of
            Nothing -> return Nothing
            Just blk -> do
                blkId <- newStmt (Block [])
                withCurId blkId (buildStmt blk)
                return (Just blkId)

        void $ appendStmt $ If pos expr (Stmt blkId) (fmap Stmt mblkIdm)

    Block stmts -> do
        id <- appendStmt (Block [])
        withCurId id (mapM_ buildStmt stmts)

    Switch pos expr cases -> do
        cases' <- forM cases $ \(pat, (Block stmts)) -> do
            blkId <- newStmt (Block [])
            withCurId blkId (mapM_ buildStmt stmts)
            return (pat, (Stmt blkId))

        void $ appendStmt (Switch pos expr cases')

    For pos expr mpat (Block stmts) -> do
        blkId <- newStmt (Block [])
        withCurId blkId (mapM_ buildStmt stmts)
        void $ appendStmt $ For pos expr mpat (Stmt blkId)

    While pos expr (Block stmts) -> do
        blkId <- newStmt (Block [])
        withCurId blkId (mapM_ buildStmt stmts)
        void $ appendStmt $ While pos expr (Stmt blkId)

    x -> error (show x)


preprocessMapper :: Elem -> DoM s Elem
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

