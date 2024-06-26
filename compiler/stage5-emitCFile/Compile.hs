module Compile where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import CGenerate
import CAst as C
import CBuilder as C
import AST as S
import Type as Type
import ASTResolved
import Symbol
import Error
import Builtin
import FindFunc
import Monad


generateAst :: MonadIO m => ASTResolved -> m (Either Error (((), GenerateState), BuilderState))
generateAst ast = runGenerate (initGenerateState ast) C.initBuilderState generate


generate :: Generate ()
generate = withErrorPrefix "generate: " $ do
    ast <- gets astResolved

    -- generate imported function externs
    forM_ (Map.toList $ funcInstanceImported ast) $ \(funcType, header) -> do
        crt <- cRettyType (S.funcRetty header)
        cats <- forM (S.funcArgs header) $ \param -> case param of
            S.Param _ _ _ -> cTypeOf param
            S.RefParam _ _ _ -> cRefTypeOf param
        appendExtern (showSymGlobal $ funcSymbol header) crt cats [C.Extern]


    -- generate functions, main is a special case
    forM_ (funcDefsTop ast) $ \symbol -> let Just func = Map.lookup symbol (funcDefsAll ast) in do
        let Just (generics, _) = Map.lookup symbol (typeDefsAll ast)
        when (generics == []) $ do
            header <- generateFunc (TypeDef symbol)



            when (symbolsCouldMatch (Sym ["main"]) (funcSymbol $ funcHeader func)) $ do
                id <- newFunction
                    Cint
                    "main"  
                    [ C.Param "argc" Cint, C.Param "argv" (Cpointer (Cpointer Cchar)) ]
                    []

                withCurID id $ do
                    --appendElem $ C.ExprStmt $ C.Call "doodad_set_args" [C.Ident "argc", C.Ident "argv"]
                    call (showSymGlobal $ funcSymbol header) []
                    void $ appendElem $ C.Return $ C.Int 0
                withCurID globalID (append id)


cRettyType :: S.Retty -> Generate C.Type
cRettyType retty = case retty of
    Retty t -> cTypeOf t
    RefRetty t -> cRefTypeOf t


generateFunc :: Type.Type -> Generate FuncHeader
generateFunc funcType = do
    isImportedInstance <- gets (Map.member funcType . funcInstanceImported . astResolved)
    isInstance <- gets (Map.member funcType . funcInstance . astResolved)

    --liftIO $ putStrLn $ "generateFunc: " ++ show funcType
    --liftIO $ putStrLn $ "isImpInst, isInst: " ++ show (isImportedInstance, isInstance)

    if isImportedInstance then do
        Just symbol <- gets (Map.lookup funcType . funcInstanceImported . astResolved)
        return symbol

    else if isInstance then do
        Just symbol <- gets (Map.lookup funcType . funcInstance . astResolved)
        return symbol

    else do
        ast <- gets astResolved
        func <- fmap fst $ runDoMExcept ast (findFunction funcType)
        let header = funcHeader func
        args <- mapM cParamOf (S.funcArgs header)
        rettyType <- cRettyType (S.funcRetty header)


        pushSymTab
        (symbol, ast') <- CGenerate.genSymbol (funcSymbol header) ast
        let header' = header { funcSymbol = symbol }
        --liftIO $ putStrLn $ "generated: " ++ show funcType
        modify $ \s -> s { astResolved = ast'
            { funcInstance = Map.insert funcType header' (funcInstance ast') } }


        id <- newFunction rettyType (showSymGlobal $ symbol) ([] ++ args) []
        withCurID id $ do
            forM_ (S.funcArgs (S.funcHeader func)) $ \arg -> do
                let name = showSymLocal (paramSymbol arg)
                case arg of
                    S.Param _ _ _ ->    define name $ Value (typeof arg) (C.Ident name)
                    S.RefParam _ _ _ -> define name $ Ref (typeof arg) (C.Ident name)
                    x -> error (show x)


            isRefRetty <- case S.funcRetty header of
                RefRetty _ -> return True
                Retty _    -> return False
            oldCurFnIsRef <- gets curFnIsRef
            modify $ \s -> s { curFnIsRef = isRefRetty }
            generateStmt (S.funcStmt func)
            modify $ \s -> s { curFnIsRef = oldCurFnIsRef }

            when (S.funcRetty header /= S.Retty Void) $ -- check to ensure function has return
                call "assert" [false]

        popSymTab
        withCurID globalID $ append id

        return header'




generateStmt :: S.Stmt -> Generate ()
generateStmt stmt = withPos stmt $ case stmt of
    S.Block stmts          -> mapM_ generateStmt stmts
    S.EmbedC _ str         -> void $ appendElem (C.Embed str)
    S.ExprStmt expr        -> void $ generateExpr expr
    S.Return _ Nothing     -> void $ appendElem (C.ReturnVoid)

    S.Return _ (Just expr) -> do
        val <- generateExpr expr
        refRetty <- gets curFnIsRef
        void $ case (refRetty, val) of
            (False, Value _ _) -> appendElem $ C.Return (valExpr val)
            (False, Ref _ _)   -> appendElem . C.Return . valExpr =<< deref val
            (True, Ref _ _)    -> appendElem $ C.Return $ refExpr val
            (True, Value _ _)  -> fail "cannot return value in reference function"

    S.Let _ (PatAnnotated (PatIdent _ symbol) patType) Nothing Nothing -> do
        base <- baseTypeOf patType
        let name = showSymLocal symbol
        define name (Value patType $ C.Ident name)
        cType <- cTypeOf patType

        void $ appendAssign cType (showSymLocal symbol) =<< initialiser base

    S.Assign pos symbol expr -> do
        val <- generateExpr expr
        case val of
            Value _ _ -> do
                let name = showSymLocal symbol
                cType <- cTypeOf (typeof val)
                void $ appendAssign cType name (valExpr val)
                define name $ Value (typeof val) (C.Ident name)

            Ref _ _ -> do
                let name = showSymLocal symbol
                cType <- cRefTypeOf (typeof val)
                void $ appendAssign cType name (refExpr val)
                define name $ Ref (typeof val) (C.Ident name)

            x -> error (show x)


    S.Data _ symbol typ Nothing -> do
        base <- baseTypeOf typ
        ctyp <- cTypeOf typ
        appendAssign ctyp (showSymLocal symbol) =<< initialiser typ
        define (showSymLocal symbol) $ Value typ $ C.Ident (showSymLocal symbol)

    S.If _ expr blk melse -> do
        val <- generateExpr expr
        if_ val $ generateStmt blk
        when (isJust melse) $ do
            elseID <- appendElem $ C.Else { elseStmts = [] }
            withCurID elseID $ generateStmt (fromJust melse)

    S.While _ expr stmt -> do
        id <- appendElem $ C.For Nothing Nothing Nothing []
        withCurID id $ do
            val <- generateExpr expr
            if_ (not_ val) $ appendElem C.Break
            generateStmt stmt
        
    x -> error (show x)


-- generateExpr should return a 're-enter-able' expression, eg 1, not func()
generateExpr :: Expr -> Generate Value
generateExpr (AExpr typ expr_) = withPos expr_ $ withTypeCheck $ case expr_ of
    S.Bool _ b             -> return $ Value typ (C.Bool b)
    S.Int _ n              -> return $ Value typ (C.Int n)
    S.Float _ f            -> return $ Value typ (C.Float f)
    S.Char _ c             -> return $ Value typ (C.Char c)
    S.Ident _ symbol       -> look (showSymLocal symbol)
    S.String _ s           -> assign "string" $ Value typ $
        C.Initialiser [C.String s, C.Int (fromIntegral $ length s)]

--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "sliceAt"]) -> do
--        val <- generateExpr expr1
--        idx <- generateExpr expr2
--        builtinSliceAt val idx
--
--    S.Call _ symbol [expr, start, end] | symbolsCouldMatch symbol (Sym ["builtin", "tableSlice"]) -> do
--        ref@(Ref _ exp) <- generateExpr expr
--        srt@(Value I64 _) <- generateExpr start
--        en@(Value I64 _) <- generateExpr end
--
--        -- TODO this is broken 
--        Apply Table [t] <- baseTypeOf ref
--        assign "slice" $ Value (Apply Slice [t]) $ C.Initialiser
--            [ C.Address (C.Subscript (C.PMember exp "r0") (valExpr srt))
--            , C.Infix C.Minus (C.PMember exp "len") (valExpr srt)
--            ] 

    S.Call _ funcType exprs -> let (TypeDef symbol, _) = unfoldType funcType in
        case (symbol, exprs) of

            (s, [expr]) | symbolsCouldMatch s (Sym ["builtin", "builtinSumEnum"]) -> do
                builtinSumEnum =<< generateExpr expr

            (s, [expr, idx]) | symbolsCouldMatch s (Sym ["builtin", "builtinSumReset"]) -> do
                val1 <- generateExpr expr
                builtinSumReset val1 =<< generateExpr idx
                return $ Value Void (C.Int 0)

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinAdd"]) -> do
                val1 <- deref =<< generateExpr expr1
                val2 <- deref =<< generateExpr expr2
                builtinAdd val1 val2

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinSubtract"]) -> do
                val1 <- deref =<< generateExpr expr1
                val2 <- deref =<< generateExpr expr2
                builtinSubtract val1 val2

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinMultiply"]) -> do
                val1 <- deref =<< generateExpr expr1
                val2 <- deref =<< generateExpr expr2
                builtinMultiply val1 val2

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinDivide"]) -> do
                val1 <- deref =<< generateExpr expr1
                val2 <- deref =<< generateExpr expr2
                builtinDivide val1 val2

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinModulo"]) -> do
                val1 <- deref =<< generateExpr expr1
                val2 <- deref =<< generateExpr expr2
                builtinModulo val1 val2

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinEqual"]) -> do
                val1 <- deref =<< generateExpr expr1
                val2 <- deref =<< generateExpr expr2
                builtinEqual val1 val2

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinLessThan"]) -> do
                val1 <- deref =<< generateExpr expr1
                val2 <- deref =<< generateExpr expr2
                builtinLessThan val1 val2

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinGreaterThan"]) -> do
                val1 <- deref =<< generateExpr expr1
                val2 <- deref =<< generateExpr expr2
                builtinGreaterThan val1 val2

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinAnd"]) -> do
                unless (typeof expr1 == typeof expr2) (error "type mismatch")
                base <- baseTypeOf expr1
                val1 <- deref =<< generateExpr expr1
                val2 <- deref =<< generateExpr expr2
                case base of
                    Type.Bool -> return $ Value (typeof val1) $
                        C.Infix C.AndAnd (valExpr val1) (valExpr val2)

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinOr"]) -> do
                unless (typeof expr1 == typeof expr2) (error "type mismatch")
                base <- baseTypeOf expr1
                val1 <- deref =<< generateExpr expr1
                val2 <- deref =<< generateExpr expr2
                case base of
                    Type.Bool -> return $ Value (typeof val1) $
                        C.Infix C.OrOr (valExpr val1) (valExpr val2)


            (s, [expr]) | symbolsCouldMatch s (Sym ["builtin", "builtinNot"]) -> do
                builtinNot =<< deref =<< generateExpr expr

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinStore"]) -> do
                check (typeof expr1 == typeof expr2) "type mismatch"
                base <- baseTypeOf expr1
                ref1@(Ref _ _) <- generateExpr expr1
                builtinStore ref1 =<< generateExpr expr2
                return $ Value Void (C.Int 0)

            (s, [expr]) | symbolsCouldMatch s (Sym ["builtin", "builtinArrayLen"]) -> do
                builtinLen =<< generateExpr expr

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinArrayAt"]) -> do
                val <- generateExpr expr1
                idx <- generateExpr expr2
                builtinArrayAt val idx

            (s, [expr]) | symbolsCouldMatch s (Sym ["builtin", "builtinTableLen"]) -> do
                builtinLen =<< generateExpr expr

            (s, [expr]) | symbolsCouldMatch s (Sym ["builtin", "builtinTableAppend"]) -> do
                val <- generateExpr expr
                Apply Table _ <- baseTypeOf val
                case val of
                    Value _ _ -> fail "isn't reference"
                    Ref _ _   -> builtinTableAppend val >> return (Value Void $ C.Int 0)

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinTableAt"]) -> do
                val <- generateExpr expr1
                idx <- generateExpr expr2
                builtinTableAt val idx

            (s, [expr]) | symbolsCouldMatch s (Sym ["builtin", "builtinSliceLen"]) -> do
                builtinLen =<< generateExpr expr

            (s, [expr1, expr2]) | symbolsCouldMatch s (Sym ["builtin", "builtinSliceAt"]) -> do
                val <- generateExpr expr1
                idx <- generateExpr expr2
                builtinSliceAt val idx

            (s, [expr]) | symbolsCouldMatch s (Sym ["builtin", "pretend"]) -> do
                Ref exprType refExpr <- generateExpr expr

                base <- baseTypeOf typ
                baseExpr <- baseTypeOf exprType
                check (base == baseExpr) ("types do not have same base: " ++ show (base, baseExpr))

                case base of
                    Apply Tuple _ ->
                        assign "ref" $ Ref typ $ C.Initialiser [C.Member refExpr "ptr"]
                    _ -> 
                        assign "ref" $ Ref typ $ C.Initialiser [refExpr]

            (s, exprs) -> do
                args <- mapM generateExpr exprs
                header <- generateFunc funcType
                let symbol = funcSymbol header
            
                argExprs <- forM (zip args $ S.funcArgs header) $ \(arg, param) -> case (arg, param) of
                    (Value _ _, S.Param _ _ _) -> return (valExpr arg)
                    (Ref _ _, S.RefParam _ _ _) -> return (refExpr arg)
                    (Ref _ _, S.Param _ _ _) -> valExpr <$> deref arg
                    (Value _ _, S.RefParam _ _ _) -> refExpr <$> reference arg
                    x -> error (show x)

                case S.funcRetty header of
                    Retty Void -> do
                        appendElem $ C.ExprStmt $ C.Call (showSymGlobal symbol) argExprs
                        return $ Value Void (C.Int 0)
                    Retty retType    -> assign "call" $ Value typ $ C.Call (showSymGlobal symbol) argExprs
                    RefRetty retType -> assign "call" $ Ref typ $ C.Call (showSymGlobal symbol) argExprs


    S.Field _ expr idx -> member idx =<< generateExpr expr

    S.Reference pos expr -> reference =<< generateExpr expr

    S.Array pos exprs -> do
        vals <- mapM deref =<< mapM generateExpr exprs
        cTyp <- cTypeOf (head vals)
        name <- fresh "array"
        appendElem $ C.Assign (Carray (length exprs) cTyp) name $ C.Initialiser (map valExpr vals)
        assign "slice" $ Value typ $ C.Initialiser
            [ C.Ident name
            , C.Int (fromIntegral $ length exprs)
            ]

    _ -> error (show expr_)
    where
        withTypeCheck :: Generate Value -> Generate Value
        withTypeCheck f = do
            r <- f
            unless (typeof r == typ) $ error ("generateExpr returned incorrect type: " ++ show (typeof r))
            return r
generateExpr x = fail $ "unresolved expression: " ++ show x
