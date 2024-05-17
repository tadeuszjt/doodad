module Compile where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import CGenerate
import CAst as C
import CBuilder as C
import CGenerate
import AST as S
import Type as Type
import ASTResolved
import Symbol
import Error
import Builtin


generateAst :: MonadIO m => ASTResolved -> m (Either Error (((), GenerateState), BuilderState))
generateAst ast = runGenerate (initGenerateState ast) C.initBuilderState generate


generate :: Generate ()
generate = withErrorPrefix "generate: " $ do
    ast <- gets astResolved

    -- generate imported function externs
    forM_ (funcInstanceImported ast) $ \func -> do
        crt <- cRettyType (S.funcRetty $ S.funcHeader func)
        cats <- forM (S.funcArgs $ S.funcHeader func) $ \param -> case param of
            S.Param _ _ _ -> cTypeOf param
            S.RefParam _ _ _ -> cRefTypeOf param
        appendExtern (showSymGlobal $ funcSymbol $ funcHeader func) crt cats [C.Extern]

    -- generate headers for this module
    forM_ (funcInstance ast) $ \func -> do
        crt <- cRettyType (S.funcRetty $ S.funcHeader func)
        cats <- forM (S.funcArgs $ S.funcHeader func) $ \param -> case param of
            S.Param _ _ _ -> cTypeOf param
            S.RefParam _ _ _ -> cRefTypeOf param
        appendExtern (showSymGlobal $ funcSymbol $ funcHeader func) crt cats []
        

    -- generate functions, main is a special case
    forM_ (funcInstance ast) $ \(func) -> do
        generateFunc False (funcSymbol $ funcHeader func) func

        when (symbolsCouldMatch (Sym ["main"]) (funcSymbol $ funcHeader func)) $ do
            id <- newFunction
                Cint
                "main"  
                [ C.Param "argc" Cint, C.Param "argv" (Cpointer (Cpointer Cchar)) ]
                []

            withCurID id $ do
                appendElem $ C.ExprStmt $ C.Call "doodad_set_args" [C.Ident "argc", C.Ident "argv"]
                call (showSymGlobal $ funcSymbol $ funcHeader func) []
                void $ appendElem $ C.Return $ C.Int 0
            withCurID globalID (append id)


cRettyType :: S.Retty -> Generate C.Type
cRettyType retty = case retty of
    Retty t -> cTypeOf t
    RefRetty t -> cRefTypeOf t


generateFunc :: Bool -> Symbol -> Func -> Generate ()
generateFunc isStatic symbol func = do
    args <- mapM cParamOf (S.funcArgs $ S.funcHeader func)
    rettyType <- cRettyType (S.funcRetty $ S.funcHeader func)

    isRefRetty <- case S.funcRetty (S.funcHeader func) of
        RefRetty _ -> return True
        Retty _    -> return False
    modify $ \s -> s { curFnIsRef = isRefRetty }

    pushSymTab

    id <- newFunction rettyType (showSymGlobal symbol) ([] ++ args) $ (if isStatic then [C.Static] else [])
    withCurID id $ do
        forM_ (S.funcArgs (S.funcHeader func)) $ \arg -> do
            let name = showSymLocal (paramSymbol arg)
            case arg of
                S.Param _ _ _ ->    define name $ Value (typeof arg) (C.Ident name)
                S.RefParam _ _ _ -> define name $ Ref (typeof arg) (C.Ident name)
                x -> error (show x)

        generateStmt (S.funcStmt func)
        when (S.funcRetty (S.funcHeader func) /= S.Retty Void) $ -- check to ensure function has return
            call "assert" [false]

    popSymTab
    withCurID globalID $ append id


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

    S.Let _ (PatAnnotated (PatIdent _ symbol) patType) Nothing Nothing -> do
        base <- baseTypeOf patType
        let name = showSymLocal symbol
        define name (Value patType $ C.Ident name)
        cType <- cTypeOf patType
        void $ appendAssign cType (showSymLocal symbol) $ C.Initialiser [C.Int 0]

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
        init <- case base of
            Apply Tuple [] -> return (C.Initialiser [])
            _              -> return (C.Initialiser [C.Int 0])
        
        ctyp <- cTypeOf typ
        appendAssign ctyp (showSymLocal symbol) init
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
        
    _ -> error "invalid statement"


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

--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "add"]) -> do
--        val1 <- deref =<< generateExpr expr1
--        val2 <- deref =<< generateExpr expr2
--        builtinAdd val1 val2
--
--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "subtract"]) -> do
--        val1 <- deref =<< generateExpr expr1
--        val2 <- deref =<< generateExpr expr2
--        builtinSubtract val1 val2
--
--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "multiply"]) -> do
--        val1 <- deref =<< generateExpr expr1
--        val2 <- deref =<< generateExpr expr2
--        builtinMultiply val1 val2
--
--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "divide"]) -> do
--        val1 <- deref =<< generateExpr expr1
--        val2 <- deref =<< generateExpr expr2
--        builtinDivide val1 val2
--
--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "modulo"]) -> do
--        val1 <- deref =<< generateExpr expr1
--        val2 <- deref =<< generateExpr expr2
--        builtinModulo val1 val2
--
--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "equal"]) -> do
--        val1 <- deref =<< generateExpr expr1
--        val2 <- deref =<< generateExpr expr2
--        builtinEqual val1 val2
--
--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "tableAt"]) -> do
--        val <- generateExpr expr1
--        idx <- generateExpr expr2
--        builtinTableAt val idx
--
--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "sliceAt"]) -> do
--        val <- generateExpr expr1
--        idx <- generateExpr expr2
--        builtinSliceAt val idx
--
--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "arrayAt"]) -> do
--        val <- generateExpr expr1
--        idx <- generateExpr expr2
--        builtinArrayAt val idx
--
--    S.Call _ symbol [expr] | symbolsCouldMatch symbol (Sym ["builtin", "tableAppend"]) -> do
--        val <- generateExpr expr
--        Apply Table _ <- baseTypeOf val
--        case val of
--            Value _ _ -> fail "isn't reference"
--            Ref _ _   -> builtinTableAppend val >> return (Value Void $ C.Int 0)
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
--
--    S.Call _ symbol [expr] | symbolsCouldMatch symbol (Sym ["builtin", "sumEnum"]) -> do
--        builtinSumEnum =<< generateExpr expr
--
--    S.Call _ symbol [expr, idx] | symbolsCouldMatch symbol (Sym ["builtin", "sumReset"]) -> do
--        val1 <- generateExpr expr
--        builtinSumReset val1 =<< generateExpr idx
--        return $ Value Void (C.Int 0)
--
--    S.Call _ symbol [expr1, expr2] | symbolsCouldMatch symbol (Sym ["builtin", "store"]) -> do
--        check (typeof expr1 == typeof expr2) "type mismatch"
--        base <- baseTypeOf expr1
--        ref1@(Ref _ _) <- generateExpr expr1
--        builtinStore ref1 =<< generateExpr expr2
--        return $ Value Void (C.Int 0)
--
--    S.Call _ symbol [expr] | symbolsCouldMatch symbol (Sym ["builtin", "tableLen"]) -> do
--        builtinLen =<< generateExpr expr
--
--    S.Call _ symbol exprs | symbolsCouldMatch (Sym ["builtin", "pretend"]) symbol -> do
--        let [expr] = exprs
--        Ref exprType refExpr <- generateExpr expr
--
--        base <- baseTypeOf typ
--        baseExpr <- baseTypeOf exprType
--        check (base == baseExpr) ("types do not have same base: " ++ show (base, baseExpr))
--
--        case base of
--            Apply Tuple _ ->
--                assign "ref" $ Ref typ $ C.Initialiser [C.Member refExpr "ptr"]
--            _ -> 
--                assign "ref" $ Ref typ $ C.Initialiser [refExpr]
--
--
--    S.Call _ symbol exprs -> do
--        check (symbolIsResolved symbol) ("unresolved function call: " ++ prettySymbol symbol)
--        callFunction symbol typ =<< mapM generateExpr exprs

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
