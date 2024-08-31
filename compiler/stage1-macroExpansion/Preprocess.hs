{-# LANGUAGE FlexibleInstances #-}
module Preprocess where

import Data.Char
import Control.Monad.Identity
import qualified Data.Map as Map
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

    EmbedC _ _ _ -> return statement
    Return _ _  -> return statement
    ExprStmt _ -> return statement
    Data _ _ _ _ -> return statement
    Feature _ _ _ _ _ _ -> return statement
    Assign _ _ _ -> return statement
    Derives _ _ _ _ -> return statement
    MacroTuple _ _ _ _ -> return statement

    Acquires pos generics typ args isRef stmt -> Acquires pos generics typ args isRef <$> unpackStmt stmt

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

    FuncDef generics (AST.Func pos symbol args retty stmt) ->
        FuncDef generics . AST.Func pos symbol args retty <$> unpackStmt stmt
    x -> error (show x)


buildPattern :: ID -> Pattern -> Expr -> DoM BuildState Expr
buildPattern defBlkId pattern expr = do
    withPos pattern $ case pattern of
        PatIgnore pos -> return (AST.Bool pos True)

        PatIdent pos symbol -> do
            withCurId defBlkId $
                appendStmt $ Let pos (PatIdent pos symbol) Nothing Nothing
            appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin",  "builtinStore"])
                [ Reference pos (Ident pos symbol)
                , expr
                ]
            return (AST.Bool pos True)

        PatLiteral literal -> do
            return $ Call (textPos pattern) (TypeDef $ Sym ["compare", "equal"]) [literal, expr]

        PatAnnotated pat patTyp -> case pat of
            PatLiteral literal -> do
                return $ Call (textPos pattern) (TypeDef $ Sym ["compare", "equal"]) [AExpr patTyp literal, expr]

            PatIdent pos symbol -> do
                withCurId defBlkId $
                    appendStmt $ Let pos (PatIdent pos symbol) Nothing Nothing
                appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin",  "builtinStore"])
                    [ Reference pos (Ident pos symbol)
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
                appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin", "builtinStore"])
                    [ Reference pos (Ident pos patMatch)
                    , guard
                    ]

            appendStmt $ If pos (Ident pos patMatch) (Stmt ifBlkId) Nothing
            return (Ident pos patMatch)


        PatTuple pos pats -> do
            symbol <- freshSym "tupleCopy"
            appendStmt (Assign pos symbol expr)

            matchSym <- freshSym "tupleMatch"
            appendStmt $ Assign pos matchSym (AST.Bool pos True)

            forM_ (zip pats [0..]) $ \(pat, i) -> do
                ifBlkId <- newStmt (Block [])
                withCurId ifBlkId $ do
                    let callType = foldType [ TypeDef (Sym ["tuple", "tuplePattern"]), Type 0, Size (length pats), Size i, Type 0]
                    match <- buildPattern defBlkId pat $ Call pos callType [Reference pos $ Ident pos symbol]
                    appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin", "builtinStore"])
                        [ Reference pos (Ident pos matchSym)
                        , match
                        ]

                appendStmt $ If pos (Ident pos matchSym) (Stmt ifBlkId) Nothing

            return (Ident pos matchSym)


        PatField pos symbol pats -> do
            --let Sym [str] = symbol
            let Sym xs = symbol
            let isSymbol   = Sym $ init xs ++ [ "is" ++ [toUpper $ head $ last xs] ++ (tail $ last xs) ]
            let fromSymbol = Sym $ init xs ++ [ "from" ++ [toUpper $ head $ last xs] ++ (tail $ last xs) ]



            exprCopy <- freshSym "exprCopy"
            appendStmt (Assign pos exprCopy expr)

            enumMatch <- freshSym "enumMatch"
            appendStmt $ Assign pos enumMatch $ Call pos (TypeDef isSymbol)
                [ Reference pos (Ident pos exprCopy)
                ]

            blkId <- newStmt (Block [])
            withCurId blkId $ do
                caseCopy <- freshSym "caseCopy"
                case pats of
                    [] -> return ()
                    _  -> void $ appendStmt $ Assign pos caseCopy $
                        Call pos (TypeDef fromSymbol) [ Reference pos (Ident pos exprCopy) ]

                match <- case pats of
                    []    -> return (AST.Bool pos True)
                    [pat] -> buildPattern defBlkId pat (Ident pos caseCopy)
                    pats  -> buildPattern defBlkId (PatTuple pos pats) (Ident pos caseCopy)

                void $ appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin", "builtinStore"])
                    [ Reference pos (Ident pos enumMatch)
                    , match
                    ]

            appendStmt $ If pos (Ident pos enumMatch) (Stmt blkId) Nothing

            return (Ident pos enumMatch)
                        

        PatSlice pos pats -> do
            exprCopy <- freshSym "exprCopy"
            appendStmt (Assign pos exprCopy expr)

            match <- freshSym "match"
            appendStmt $ Assign pos match $ Call pos (TypeDef $ Sym ["builtin", "builtinEqual"])
                [ Call pos (TypeDef $ Sym ["Pattern", "sliceLen"]) [Reference pos (Ident pos exprCopy)]
                , AST.Int pos (fromIntegral $ length pats)
                ]


            forM_ (zip pats [0..]) $ \(pat, i) -> do
                ifBlkId <- newStmt (Block [])
                withCurId ifBlkId $ do
                    patMatch <- buildPattern defBlkId pat $ Call pos (TypeDef $ Sym ["Pattern", "sliceAt"])
                        [ Reference pos (Ident pos exprCopy)
                        , AST.Int pos (fromIntegral i)
                        ]

                    appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin", "builtinStore"])
                        [ Reference pos (Ident pos match)
                        , patMatch
                        ]

                appendStmt $ If pos (Ident pos match) (Stmt ifBlkId) Nothing

            return (Ident pos match)
                

        x -> error (show x)


buildCondition :: ID -> Expr -> DoM BuildState Expr
buildCondition defBlkId expression = withPos expression $ case expression of
    Call pos _ _       -> return $ AExpr Type.Bool expression
    Match pos expr pat -> buildPattern defBlkId pat expr
    AST.Bool _ _       -> return expression
    Ident pos _        -> return $ AExpr Type.Bool expression

    AExpr exprType expr -> do
        check (exprType == Type.Bool) "condition type must be bool"
        buildCondition defBlkId expr

    Reference pos expr -> return $ AExpr Type.Bool expression
        

    x -> error (show x)


field :: Type -> Integer -> Expr -> Expr
field fieldType i expr = AExpr fieldType $ Call (textPos expr) typ [expr]
    where
        typ = foldType [TypeDef (Sym ["builtin", "builtinField"]), Size (fromIntegral i), Type 0, Type 0]
    


buildStmt :: Stmt -> DoM BuildState ()
buildStmt statement = withPos statement $ case statement of
    ExprStmt expr     -> void $ appendStmt (ExprStmt expr)
    Typedef _ _ _ _   -> void $ appendStmt statement
    EmbedC _ _ _      -> void $ appendStmt statement
    Return pos mexpr  -> void $ appendStmt statement
    Data _ _ _ _      -> void $ appendStmt statement
    Feature _ _ _ _ _ _ -> void $ appendStmt statement
    Derives _ _ _ _   -> void $ appendStmt statement
    --MacroTuple _ _ _ _ -> void $ appendStmt statement

    Acquires pos generics typ args isRef (Block stmts) -> do
        blockId <- newStmt (Block [])
        withCurId blockId (mapM_ buildStmt stmts)
        void $ appendStmt $ Acquires pos generics typ args isRef (Stmt blockId)

    FuncDef generics (AST.Func pos symbol args retty (Block stmts)) -> do
        appendStmt $ Feature pos generics [] symbol (map typeof args) (typeof retty)

        acqIsRef <- case retty of
            RefRetty _ -> return True
            Retty _    -> return False

        blockId <- newStmt (Block [])
        appendStmt $ Acquires pos generics (foldl Apply (TypeDef symbol) $ map TypeDef generics) args acqIsRef (Stmt blockId)

        --appendStmt $ FuncDef generics (AST.Func header (Stmt blockId))
        withCurId blockId (mapM_ buildStmt stmts)

    Let pos (PatIdent _ _) mexpr Nothing -> case mexpr of
        Nothing -> void $ appendStmt statement
        Just expr -> void $ appendStmt statement

    Let pos pat mexpr Nothing -> do
        case mexpr of
            Just expr -> do
                curId <- getCurId
                boolExpr <- buildPattern curId pat expr
                void $ appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["assert"]) [boolExpr]

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
                _ -> return $ Call pos (TypeDef $ Sym ["builtin", "builtinNot"]) [Ident pos $ caseSyms !! (i - 1)]

            blkId <- newStmt (Block [])
            withCurId blkId $ do
                match <- buildPattern blkId pat (Ident pos copySym)
                appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin", "builtinStore"])
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
        appendStmt $ Assign pos idx $ AExpr I64 (AST.Int pos 0)
        appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin", "builtinStore"])
            [ Reference pos (Ident pos idx)
            , Call pos (TypeDef $ Sym ["for", "forBegin"]) [ Reference pos (Ident pos exprCopy) ]
            ]

        let whileCnd = Call pos (TypeDef $ Sym ["builtin", "builtinLessThan"])
                [ (Ident pos idx)
                , Call pos (TypeDef $ Sym ["for", "forEnd"]) [Reference pos $ Ident pos exprCopy]
                ]

        blkId <- newStmt (Block [])
        withCurId blkId $ do
            match <- case mpat of
                Just pat -> buildPattern blkId pat $ 
                    Call pos (TypeDef $ Sym ["for", "forAt"])
                        [ Reference pos (Ident pos exprCopy)
                        , (Ident pos idx)
                        ]
                Nothing -> return (AST.Bool pos True)

            trueBlkId <- newStmt (Block [])
            withCurId trueBlkId $ do
                (mapM_ buildStmt stmts)
                appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin", "builtinStore"])
                    [ Reference pos (Ident pos idx)
                    , Call pos (TypeDef $ Sym ["builtin", "builtinAdd"]) [ (Ident pos idx) , AST.Int pos 1 ]
                    ]

            falseBlkId <- newStmt (Block [])
            withCurId falseBlkId $ do
                appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin", "builtinStore"])
                    [ Reference pos (Ident pos idx)
                    , AST.Int pos 9999999 -- TODO
                    ]

            appendStmt $ If pos match (Stmt trueBlkId) (Just $ Stmt falseBlkId)

        void $ appendStmt $ While pos whileCnd (Stmt blkId)

    While pos expr (Block stmts) -> do
        loop <- freshSym "loop"
        appendStmt $ Assign pos loop (AST.Bool pos True)
        id <- newStmt (Block [])
        withCurId id $ do
            cnd <- buildCondition id expr

            trueBlkId <- newStmt (Block [])
            falseBlkId <- newStmt (Block [])

            withCurId trueBlkId (mapM buildStmt stmts)
            withCurId falseBlkId $ do
                appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin", "builtinStore"])
                    [ Reference pos (Ident pos loop)
                    , AST.Bool pos False
                    ]

            appendStmt $ If pos cnd (Stmt trueBlkId) (Just $ Stmt falseBlkId)

        void $ appendStmt $ While pos (Ident pos loop) (Stmt id)


    MacroTuple pos generics symbol fields -> do
        void $ appendStmt $ Typedef pos generics symbol (foldl Apply Tuple $ map snd fields)
        forM_ (zip fields [0..]) $ \((fieldSymbol, fieldType), i) -> do
            -- write field feature
            let a = Sym ["A"]
            let b = Sym ["B"]
            appendStmt $ Feature pos [a, b] [(a, b)] fieldSymbol [TypeDef a] (TypeDef b)

            -- write acquires
            let typ = foldType (TypeDef symbol : map TypeDef generics)
            let acq = foldType [TypeDef fieldSymbol, typ, fieldType]
            blkId <- newStmt (Block [])
            appendStmt $ Acquires pos generics acq [RefParam pos (Sym ["x"]) Void] True (Stmt blkId)
            withCurId blkId $ do
                appendStmt $ Return pos . Just $ field fieldType i (Ident pos $ Sym ["x"])
                
            blkId2 <- newStmt (Block [])
            let acq2 = foldType [TypeDef fieldSymbol, Apply Table typ, Apply Slice fieldType]
            appendStmt $ Acquires pos generics acq2 [RefParam pos (Sym ["x"]) Void] False (Stmt blkId2)
            withCurId blkId2 $ do
                appendStmt $ Return pos . Just $ field (Apply Slice fieldType) i (Ident pos $ Sym ["x"])


    Enum pos generics symbol cases -> do
        caseTypes <- forM cases $ \(symbol, ts) -> case ts of
            [t] -> return t
            ts -> return $ foldl Apply Tuple ts

        let sumType = foldl Apply (TypeDef symbol) (map TypeDef generics)
        void $ appendStmt $ Typedef pos generics symbol (foldl Apply Sum caseTypes)

        -- write isCase0, isCase1 functions
        forM_ (zip cases [0..]) $ \( (Sym [str], ts) , i) -> do
            let name = Sym ["is" ++ (toUpper $ head str) : (tail str) ]
            blkId <- newStmt (Block [])
            withCurId blkId $
                appendStmt $ Return pos $ Just $ Call pos (TypeDef $ Sym ["builtin", "builtinEqual"])
                    [ Call pos (TypeDef $ Sym ["builtin", "builtinSumEnum"]) [Reference pos $ Ident pos $ Sym ["en"]]
                    , AST.Int pos i
                    ]

            -- feature{N, T, G} field0(A) B
            let (t) = (Sym ["T"])
            appendStmt $ Feature pos [t] [] name [TypeDef t] Type.Bool
            -- acquires{A, B}  field0{0, MyType{A, B}, MyType.0} (a&) -> &
            let acq = foldl Apply (TypeDef name) [sumType]
            appendStmt $ Acquires pos generics acq [RefParam pos (Sym ["en"]) Void] False (Stmt blkId)

        -- write case0, case1 constructors
        forM_ (zip cases [0..]) $ \( (Sym [str], ts) , i) -> do
            let name = Sym [str]

            blkId <- newStmt (Block [])
            withCurId blkId $ do
                appendStmt $ Let pos (PatIdent pos $ Sym ["en"]) Nothing Nothing
                appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["builtin", "builtinSumReset"])
                    [ Reference pos (Ident pos $ Sym ["en"])
                    , AST.Int pos i
                    ]

                case ts of
                    [t] -> void $ appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["store"])
                        [ Reference pos (field t i (Ident pos $ Sym ["en"]))
                        , Ident pos (Sym ["a0"])
                        ]
                    ts -> forM_ (zip ts [0..]) $ \(t, j) -> do
                        appendStmt $ ExprStmt $ Call pos (TypeDef $ Sym ["store"])
                            [ Reference pos (field (ts !! j) (fromIntegral j) (field (foldType (Tuple : ts)) i (Ident pos $ Sym ["en"])))
                            , Ident pos (Sym ["a" ++ show j])
                            ]

                appendStmt $ Return pos $ Just (Ident pos $ Sym ["en"])

            args <- forM (zip ts [0..]) $ \(t, j) -> do
                return $ Param pos (Sym ["a" ++ show j]) t

            appendStmt $ Feature pos generics [] name ts sumType
            let acq = foldl Apply (TypeDef name) (map TypeDef generics)
            appendStmt $ Acquires pos generics acq args False (Stmt blkId)


        -- write fromCase0, fromCase1 accessors
        forM_ (zip cases [0..]) $ \( (Sym [str], ts) , i) -> do
            let name = Sym ["from" ++ (toUpper $ head str) : (tail str) ]
            let fieldType = case ts of
                    [t] -> t
                    ts  -> foldl Apply Tuple ts

            -- feature{N, T, G} field0(A) B
            let (n, t, g) = (Sym ["N"], Sym ["T"], Sym ["G"])
            appendStmt $ Feature pos [n, g, t] [(n, g), (t, n)] name [TypeDef t] (TypeDef g)

            -- acquires{A, B}  field0{0, MyType{A, B}, MyType.0} (a&) -> &
            let acq = foldl Apply (TypeDef name) [Size (fromIntegral i), fieldType, sumType]
            blkId <- newStmt (Block [])
            appendStmt $ Acquires pos generics acq [RefParam pos (Sym ["x"]) Void] True (Stmt blkId)
            withCurId blkId $ do
                appendStmt $ Return pos . Just $ field fieldType i (Ident pos $ Sym ["x"])

    x -> error (show x)


preprocessMapper :: Elem -> DoM s Elem
preprocessMapper element = case element of
    ElemStmt (Let pos pattern mexpr (Just blk)) -> do
        return $ ElemStmt $ Block [Let pos pattern mexpr Nothing, blk]

    _ -> return element

