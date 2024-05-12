{-# LANGUAGE FlexibleInstances #-}
module Preprocess where

import Data.Char
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
    Stmt id -> unpackStmt =<< gets ((Map.! id) . statements)

    EmbedC _ _ -> return statement
    Return _ _  -> return statement
    ExprStmt _ -> return statement
    Data _ _ _ _ -> return statement
    Feature _ _ _ -> return statement
    Assign _ _ _ -> return statement

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
            appendStmt $ Assign pos patMatch match

            ifBlkId <- newStmt (Block [])
            withCurId ifBlkId $ do
                guard <- buildCondition defBlkId guardExpr
                appendStmt $ ExprStmt $ Builtin pos "builtin_store"
                    [ Reference pos (Ident pos patMatch)
                    , guard
                    ]

            appendStmt $ If pos (Ident pos patMatch) (Stmt ifBlkId) Nothing
            return (Ident pos patMatch)


        PatTuple pos pats -> do
            let syms = ["first", "second", "third", "fourth"]
        
            symbol <- freshSym "tupleCopy"
            appendStmt (Assign pos symbol expr)

            matchSym <- freshSym "tupleMatch"
            appendStmt $ Assign pos matchSym (AST.Bool pos True)

            forM_ (zip pats [0..]) $ \(pat, i) -> do
                ifBlkId <- newStmt (Block [])
                withCurId ifBlkId $ do
                    match <- buildPattern defBlkId pat $
                        Call pos (Sym [syms !! i]) [Reference pos $ Ident pos symbol]
                    appendStmt $ ExprStmt $ Builtin pos "builtin_store"
                        [ Reference pos (Ident pos matchSym)
                        , match
                        ]

                appendStmt $ If pos (Ident pos matchSym) (Stmt ifBlkId) Nothing

            return (Ident pos matchSym)

        PatSlice pos pats -> do
            exprCopy <- freshSym "exprCopy"
            appendStmt (Assign pos exprCopy expr)

            match <- freshSym "match"
            appendStmt $ Assign pos match $ Builtin pos "builtin_equal"
                [ Call pos (Sym ["Pattern", "sliceLen"]) [Reference pos (Ident pos exprCopy)]
                , AST.Int pos (fromIntegral $ length pats)
                ]


            forM_ (zip pats [0..]) $ \(pat, i) -> do
                ifBlkId <- newStmt (Block [])
                withCurId ifBlkId $ do
                    patMatch <- buildPattern defBlkId pat $ Call pos (Sym ["Pattern", "sliceAt"])
                        [ Reference pos (Ident pos exprCopy)
                        , AST.Int pos (fromIntegral i)
                        ]

                    appendStmt $ ExprStmt $ Builtin pos "builtin_store"
                        [ Reference pos (Ident pos match)
                        , patMatch
                        ]

                appendStmt $ If pos (Ident pos match) (Stmt ifBlkId) Nothing

            return (Ident pos match)
                



        x -> error (show x)


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
        appendStmt $ FuncDef (Func header (Stmt blockId))
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
        copySym <- freshSym "expr"
        appendStmt (Assign pos copySym expr)

        caseSyms <- replicateM (length cases) (freshSym "caseMatch")

        forM_ (zip cases [0..]) $ \((pat, Block stmts), i) -> do
            appendStmt $ Assign pos (caseSyms !! i) (AST.Bool pos False)

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
        exprCopy <- freshSym "exprCopy"
        appendStmt (Assign pos exprCopy expr)

        idx <- freshSym "idx"
        appendStmt $ Assign pos idx (AST.Int pos 0)
        appendStmt $ ExprStmt $ Call pos (Sym ["builtin_store"])
            [ Reference pos (Ident pos idx)
            , Call pos (Sym ["For", "begin"]) [ Reference pos (Ident pos exprCopy) ]
            ]

        let whileCnd = Call pos (Sym ["Compare", "less"])
                [ (Ident pos idx)
                , Call pos (Sym ["For", "end"]) [Reference pos $ Ident pos exprCopy]
                ]

        blkId <- newStmt (Block [])
        withCurId blkId $ do
            match <- case mpat of
                Just pat -> buildPattern blkId pat $ Call pos (Sym ["For", "forAt"])
                    [ Reference pos (Ident pos exprCopy)
                    , (Ident pos idx)
                    ]
                Nothing -> return (AST.Bool pos True)

            trueBlkId <- newStmt (Block [])
            withCurId trueBlkId $ do
                (mapM_ buildStmt stmts)
                appendStmt $ ExprStmt $ Call pos (Sym ["builtin_store"])
                    [ Reference pos (Ident pos idx)
                    , Call pos (Sym ["builtin_add"]) [ (Ident pos idx) , AST.Int pos 1 ]
                    ]

            falseBlkId <- newStmt (Block [])
            withCurId falseBlkId $ do
                appendStmt $ ExprStmt $ Call pos (Sym ["builtin_store"])
                    [ Reference pos (Ident pos idx)
                    , AST.Int pos 9999999 -- TODO
                    ]

            appendStmt $ If pos match (Stmt trueBlkId) (Just $ Stmt falseBlkId)

        void $ appendStmt $ While pos whileCnd (Stmt blkId)

    For pos expr mpat (Block stmts) -> do
        blkId <- newStmt (Block [])
        withCurId blkId (mapM_ buildStmt stmts)
        void $ appendStmt $ For pos expr mpat (Stmt blkId)

    While pos expr (Block stmts) -> do
        loop <- freshSym "loop"
        appendStmt $ Assign pos loop (AST.Bool pos True)
        id <- appendStmt (Block [])
        withCurId id $ do
            cnd <- buildCondition id expr

            trueBlkId <- newStmt (Block [])
            falseBlkId <- newStmt (Block [])

            withCurId trueBlkId (mapM buildStmt stmts)
            withCurId falseBlkId $ do
                appendStmt $ ExprStmt $ Builtin pos "builtin_store"
                    [ Reference pos (Ident pos loop)
                    , AST.Bool pos False
                    ]

            appendStmt $ If pos cnd (Stmt trueBlkId) (Just $ Stmt falseBlkId)

        void $ appendStmt $ While pos (Ident pos loop) (Stmt id)

    Enum pos generics symbol cases -> do
        caseTypes <- forM cases $ \symbol -> return $ TypeApply (Sym ["Tuple"]) []
        void $ appendStmt $ Typedef pos generics symbol $ TypeApply (Sym ["Sum"]) caseTypes

        forM_ (zip cases [0..]) $ \(Sym [str], i) -> do
            blkId <- newStmt (Block [])
            withCurId blkId $
                appendStmt $ Return pos $ Just $ Builtin pos "builtin_equal"
                    [ Builtin pos "builtin_sum_enum" [Reference pos $ Ident pos $ Sym ["en"]]
                    , AST.Int pos i
                    ]

            let header = FuncHeader
                    { funcSymbol = Sym [sym symbol, "is" ++ (toUpper (head str) : tail str)]
                    , funcRetty  = Retty Type.Bool
                    , funcArgs   =
                        [RefParam pos (Sym ["en"]) (TypeApply symbol $ map (\x -> TypeApply x []) generics)]
                    , funcGenerics = generics
                    , funcPos = pos
                    }
            appendStmt $ FuncDef $ Func header (Stmt blkId)

        forM_ (zip cases [0..]) $ \(Sym [str], i) -> do
            blkId <- newStmt (Block [])
            withCurId blkId $ do
                appendStmt $ Let pos (PatIdent pos $ Sym ["en"]) Nothing Nothing
                appendStmt $ ExprStmt $ Builtin pos "builtin_sum_reset"
                    [ Reference pos (Ident pos $ Sym ["en"])
                    , AST.Int pos i
                    ]
                appendStmt $ Return pos $ Just (Ident pos $ Sym ["en"])

            let header = FuncHeader
                    { funcSymbol = Sym [sym symbol, str]
                    , funcRetty  = Retty $ TypeApply symbol (map (\x -> TypeApply x []) generics)
                    , funcArgs   = []
                    , funcGenerics = generics
                    , funcPos = pos
                    }
            appendStmt $ FuncDef $ Func header (Stmt blkId)

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

