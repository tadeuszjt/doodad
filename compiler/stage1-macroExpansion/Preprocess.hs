{-# LANGUAGE FlexibleInstances #-}
module Preprocess where

import Data.Char
import Control.Monad.Identity
import qualified Data.Map as Map
import Control.Monad.State

import Monad
import AST
import Symbol
import InstBuilder
import AstBuilder
import Error
import Type


preprocess :: AST -> DoM s AstBuilderState
preprocess ast = do
    ((), buildState) <- runDoMExcept (initBuildState (astModuleName ast) (astImports ast)) preprocess'
    return (astBuilderState buildState)
    where
        preprocess' :: DoM BuildState ()
        preprocess' = do
            --stmts' <- mapM (mapStmtM preprocessMapper) (astStmts ast)
            forM_ (astStmts ast) $ \stmt -> case stmt of
                    Function _ _ _ _ _ -> addTopStmt (TopStmt stmt)
                    Derives _ _ _ _     -> addTopStmt (TopStmt stmt)
                    Typedef _ _ _ _     -> addTopStmt (TopStmt stmt)
                    x                   -> buildTopStatement x


data BuildState = BuildState 
    { astBuilderState :: AstBuilderState
    , instBuilderState :: InstBuilderState
    , supply          :: Int
    }


initBuildState name imports = BuildState
    { astBuilderState = initAstBuilderState name imports
    , instBuilderState = initInstBuilderState
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
    liftAstBuilderState (StateT s) = do
        astState <- gets astBuilderState
        let (a, astState') = runIdentity (s astState)
        modify $ \s -> s { astBuilderState = astState' }
        return a

instance MonadInstBuilder (DoM BuildState) where
    liftInstBuilderState (StateT s) = do
        instState <- gets instBuilderState
        let (a, instState') = runIdentity (s instState)
        modify $ \s -> s { instBuilderState = instState' }
        return a


buildInst :: DoM BuildState a -> DoM BuildState InstBuilderState
buildInst f = do
    modify $ \s -> s { instBuilderState = initInstBuilderState }
    withCurId 0 (void f)
    gets instBuilderState


buildTopStatement :: Stmt -> DoM BuildState ()
buildTopStatement statement = case statement of
    MacroTuple pos generics symbol fields -> do
        void $ addTopStmt $ TopStmt $ Typedef pos generics symbol (foldl Apply Tuple $ map snd fields)
        forM_ (zip fields [0..]) $ \((fieldSymbol, fieldType), i) -> do
            -- write field feature
            let a = Sym ["A"]
            let b = Sym ["B"]
            addTopStmt $ TopStmt $ Function pos [a, b] [(a, b)] fieldSymbol $ foldType [Type.Func, TypeDef b, TypeDef a]

            -- write instance
            let typ = foldType (TypeDef symbol : map TypeDef generics)
            let acq = foldType [TypeDef fieldSymbol, typ, fieldType]
            addTopStmt . TopInst pos generics acq [RefParam pos (Sym ["x"]) Void] (RefRetty Void) =<<
                buildInst (appendStmt $ Return pos . Just $ field fieldType i (Ident pos $ Sym ["x"]))

            let acq2 = foldType [TypeDef fieldSymbol, Apply Table typ, Apply Slice fieldType]
            addTopStmt . TopInst pos generics acq2 [RefParam pos (Sym ["x"]) Void] (Retty Void) =<<
                buildInst (appendStmt $ Return pos . Just $ field (Apply Slice fieldType) i (Ident pos $ Sym ["x"]))

    Instance pos generics typ args isRef (Block stmts) -> do
        retty <- case isRef of
            False -> return (Retty Void)
            True  -> return (RefRetty Void)
        addTopStmt . TopInst pos generics typ args retty =<< buildInst (mapM_ buildStmt stmts)

    FuncInst pos generics symbol args retty (Block stmts) -> do
        addTopStmt $ TopStmt $ Function pos generics [] symbol $ foldType (Type.Func : typeof retty : map typeof args)
        inst' <- buildInst (mapM_ buildStmt stmts)
        addTopStmt $ TopInst pos generics (foldl Apply (TypeDef symbol) $ map TypeDef generics) args retty inst'


    Enum pos generics symbol cases -> do
        caseTypes <- forM cases $ \(symbol, ts) -> case ts of
            [t] -> return t
            ts -> return $ foldl Apply Tuple ts

        let sumType = foldl Apply (TypeDef symbol) (map TypeDef generics)
        void $ addTopStmt $ TopStmt $ Typedef pos generics symbol (foldl Apply Sum caseTypes)

        -- write isCase0, isCase1 functions
        forM_ (zip cases [0..]) $ \( (Sym [str], ts) , i) -> do
            let name = Sym ["is" ++ (toUpper $ head str) : (tail str) ]
            inst' <- buildInst $ appendStmt $ Return pos $ Just $
                Call pos (TypeDef $ Sym ["builtin", "builtinEqual"])
                    [ Call pos (TypeDef $ Sym ["builtin", "builtinSumEnum"]) [Reference pos $ Ident pos $ Sym ["en"]]
                    , AST.Int pos i
                    ]

            -- feature{N, T, G} field0(A) B
            let (t) = (Sym ["T"])
            addTopStmt $ TopStmt $ Function pos [t] [] name $ foldType [Type.Func, Type.Bool, TypeDef t]
            -- instance{A, B}  field0{0, MyType{A, B}, MyType.0} (a&) -> &
            let acq = foldl Apply (TypeDef name) [sumType]
            addTopStmt $ TopInst pos generics acq [RefParam pos (Sym ["en"]) Void] (Retty Void) inst'

        -- write case0, case1 constructors
        forM_ (zip cases [0..]) $ \( (Sym [str], ts) , i) -> do
            let name = Sym [str]

            inst' <- buildInst $ do
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

            args <- forM (zip ts [0..]) $ \(t, j) -> return $ Param pos (Sym ["a" ++ show j]) t
            addTopStmt $ TopStmt $ Function pos generics [] name $ foldType (Type.Func: sumType :ts)
            let acq = foldl Apply (TypeDef name) (map TypeDef generics)
            addTopStmt $ TopInst pos generics acq args (Retty Void) inst'


        -- write fromCase0, fromCase1 accessors
        forM_ (zip cases [0..]) $ \( (Sym [str], ts) , i) -> do
            let name = Sym ["from" ++ (toUpper $ head str) : (tail str) ]
            let fieldType = case ts of
                    [t] -> t
                    ts  -> foldl Apply Tuple ts

            -- feature{N, T, G} field0(A) B
            let (n, t, g) = (Sym ["N"], Sym ["T"], Sym ["G"])
            addTopStmt $ TopStmt $ Function pos [n, g, t] [(n, g), (t, n)] name (foldType [Type.Func, TypeDef g, TypeDef t])

            -- instance{A, B}  field0{0, MyType{A, B}, MyType.0} (a&) -> &
            let acq = foldl Apply (TypeDef name) [Size (fromIntegral i), fieldType, sumType]
            inst' <- buildInst $ appendStmt $ Return pos . Just $
                field fieldType i (Ident pos $ Sym ["x"])
            addTopStmt $ TopInst pos generics acq [RefParam pos (Sym ["x"]) Void] (RefRetty Void) inst'

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
    

buildBlock :: Stmt -> DoM BuildState ID
buildBlock (Block stmts) = do
    id <- newStmt (Block [])
    withCurId id $ mapM buildStmt stmts
    return id
buildBlock stmt = do
    id <- newStmt (Block [])
    withCurId id $ buildStmt stmt
    return id


buildStmt :: Stmt -> DoM BuildState ()
buildStmt statement = withPos statement $ case statement of
    ExprStmt expr     -> void $ appendStmt (ExprStmt expr)
    EmbedC _ _ _      -> void $ appendStmt statement
    Return pos mexpr  -> void $ appendStmt statement

    Block stmts -> do
        id <- appendStmt (Block [])
        withCurId id (mapM_ buildStmt stmts)

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

            trueId <- buildBlock blk
            falseId <- traverse buildBlock mblk
            void $ appendStmt $ If pos cnd (Stmt trueId) (fmap Stmt falseId)

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
        appendStmt $ Let pos (PatIdent pos idx) Nothing Nothing
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

    x -> error (show x)


--preprocessMapper :: Elem -> DoM s Elem
--preprocessMapper element = case element of
--    ElemStmt (Let pos pattern mexpr (Just blk)) -> do
--        return $ ElemStmt $ Block [Let pos pattern mexpr Nothing, blk]
--
--    _ -> return element
--
