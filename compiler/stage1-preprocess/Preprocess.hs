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
import Type


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


freshSym :: String -> DoM BuildState Symbol
freshSym suggestion = do
    id <- fresh
    return (Sym ["_" ++ suggestion ++ show id])


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


buildPattern :: ID -> Pattern -> Expr -> DoM BuildState Expr
buildPattern defBlkId pattern expr = do
    withPos pattern $ case pattern of
        PatIgnore pos -> return (AST.Bool pos True)

        PatIdent pos symbol -> do
            withCurId defBlkId $
                appendStmt $ Let pos (PatIdent pos symbol) Nothing Nothing
            appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"])
                [ Reference pos (Ident pos symbol)
                , expr
                ]
            return (AST.Bool pos True)

        PatLiteral literal -> do
            return $ Call (textPos pattern) (Sym ["Compare", "equal"]) [literal, expr]

        PatAnnotated pat patTyp -> case pat of
            PatLiteral literal -> do
                return $ Call (textPos pattern) (Sym ["Compare", "equal"]) [AExpr patTyp literal, expr]

            PatIdent pos symbol -> do
                withCurId defBlkId $ 
                    appendStmt $ Let pos (PatIdent pos symbol) Nothing Nothing
                appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"])
                    [ Reference pos (AExpr patTyp $ Ident pos symbol)
                    , expr
                    ]
                return (AST.Bool pos True)

            x -> error (show x)


        PatGuarded pos pat guardExpr -> do
            match <- buildPattern defBlkId pat expr

            patMatch <- freshSym "patMatch"
            appendStmt $ Let pos (PatIdent pos patMatch) Nothing Nothing
            appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"])
                [ Reference pos (Ident pos patMatch)
                , match
                ]

            ifBlkId <- newStmt (Block [])
            withCurId ifBlkId $ do
                guard <- buildCondition defBlkId guardExpr
                appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"])
                    [ Reference pos (Ident pos patMatch)
                    , guard
                    ]

            appendStmt $ If pos (Ident pos patMatch) (Stmt ifBlkId) Nothing
            return (Ident pos patMatch)


        PatTuple pos pats -> do
            let syms = ["first", "second", "third", "fourth"]

            symbol <- freshSym "tupleCopy"
            withCurId defBlkId $
                appendStmt $ Let pos (PatIdent pos symbol) Nothing Nothing

            appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"]) [Reference pos (Ident pos symbol), expr]


            matchSym <- freshSym "tupleMatch"
            appendStmt $ Let pos (PatIdent pos matchSym) Nothing Nothing
            appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"]) [Reference pos (Ident pos matchSym), AST.Bool pos True]

            forM_ (zip pats [0..]) $ \(pat, i) -> do
                ifBlkId <- newStmt (Block [])
                withCurId ifBlkId $ do
                    b <- buildPattern defBlkId pat $
                        Call pos (Sym [syms !! i]) [Reference pos $ Ident pos symbol]
                    appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"]) [Reference pos (Ident pos matchSym), b]

                appendStmt $ If pos (Ident pos matchSym) (Stmt ifBlkId) Nothing

            return (Ident pos matchSym)

        x -> fail (show x)


buildCondition :: ID -> Expr -> DoM BuildState Expr
buildCondition defBlkId expression = withPos expression $ case expression of
    Call pos _ _       -> return $ AExpr Type.Bool $ Call pos (Sym ["Construct", "construct"]) [expression]
    Match pos expr pat -> buildPattern defBlkId pat expr
    AST.Bool _ _       -> return expression
    Ident pos _        -> return $ AExpr Type.Bool $ Call pos (Sym ["Construct", "construct"]) [expression]

    AExpr exprType expr -> do
        check (exprType == Type.Bool) "condition type must be bool"
        buildCondition defBlkId expr

    Reference pos expr -> return $ AExpr Type.Bool $ Call pos (Sym ["Construct", "construct"]) [expression]
        

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

    Let pos pat mexpr Nothing -> do
        case mexpr of
            Just expr -> do
                curId <- getCurId
                boolExpr <- buildPattern curId pat expr
                void $ appendStmt $ ExprStmt $ Call pos (Sym ["assert", "assert"]) [boolExpr]

            Nothing -> do
                void $ appendStmt (Let pos pat Nothing Nothing)

    If pos expr blk mblk -> do
        id <- appendStmt (Block [])
        withCurId id $ do -- new scope for if condition
            cnd <- buildCondition id expr

            trueBlkId <- newStmt (Block [])
            withCurId trueBlkId (buildStmt blk)

            falseBlkIdm <- case mblk of
                Nothing -> return Nothing
                Just blk -> do
                    falseBlkId <- newStmt (Block [])
                    withCurId falseBlkId (buildStmt blk)
                    return (Just falseBlkId)

            void $ appendStmt $ If pos cnd (Stmt trueBlkId) (fmap Stmt falseBlkIdm)

    Block stmts -> do
        id <- appendStmt (Block [])
        withCurId id (mapM_ buildStmt stmts)

    Switch pos expr cases -> do
        copySym <- freshSym "exprCopy"
        appendStmt $ Let pos (PatIdent pos copySym) Nothing Nothing
        appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"])
            [ Reference pos (Ident pos copySym)
            , expr
            ]

        caseSyms <- replicateM (length cases) (freshSym "caseMatch")

        forM_ (zip cases [0..]) $ \((pat, Block stmts), i) -> do
            appendStmt $ Let pos (PatIdent pos $ caseSyms !! i) Nothing Nothing

            lastCaseFailed <- case i of
                0 -> return (AST.Bool pos True)
                _ -> return $ Call pos (Sym ["Boolean", "not"]) [Ident pos $ caseSyms !! (i - 1)]

            blkId <- newStmt (Block [])
            withCurId blkId $ do
                match <- buildPattern blkId pat (Ident pos copySym)
                appendStmt $ ExprStmt $ Call pos (Sym ["Store", "store"])
                    [ Reference pos (Ident pos $ caseSyms !! i)
                    , match
                    ]

                trueBlkId <- newStmt (Block [])
                withCurId trueBlkId (mapM_ buildStmt stmts)
                appendStmt $ If pos match (Stmt trueBlkId) Nothing

            appendStmt $ If pos lastCaseFailed (Stmt blkId) Nothing

    For pos expr mpat (Block stmts) -> do
        blkId <- newStmt (Block [])
        withCurId blkId (mapM_ buildStmt stmts)
        void $ appendStmt $ For pos expr mpat (Stmt blkId)

    While pos expr (Block stmts) -> do
        id <- appendStmt (Block [])
        withCurId id $ do
            cnd <- buildCondition id expr
            blkId <- newStmt (Block [])
            withCurId blkId (mapM buildStmt stmts)
            void $ appendStmt $ While pos cnd (Stmt blkId)

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

    _ -> return element

