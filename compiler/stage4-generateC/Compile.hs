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
generateAst ast = runGenerate
    (initGenerateState ast)
    C.initBuilderState
    generate


generate :: Generate ()
generate = withErrorPrefix "generate: " $ do
    ast <- gets astResolved

    -- generate imported function externs
    forM_ (funcInstanceImported ast) $ \(func) -> do
        crt <- cRettyType (S.funcRetty $ S.funcHeader func)
        cats <- forM (S.funcArgs $ S.funcHeader func) $ \param -> case param of
            S.Param _ _ _ -> cTypeOf param
            S.RefParam _ _ _ -> cRefTypeOf param
            x -> error (show x)
        appendExtern (showSymGlobal $ funcSymbol $ funcHeader func) crt cats [C.Extern]

    -- generate headers for this module
    forM_ (funcInstance ast) $ \(func) -> do
        crt <- cRettyType (S.funcRetty $ S.funcHeader func)
        cats <- forM (S.funcArgs $ S.funcHeader func) $ \param -> case param of
            S.Param _ _ _ -> cTypeOf param
            S.RefParam _ _ _ -> cRefTypeOf param
            x -> error (show x)
        appendExtern (showSymGlobal $ funcSymbol $ funcHeader func) crt cats []
        

    -- generate functions, main is a special case
    forM_ (funcInstance ast) $ \(func) -> do
        generateFunc False (funcSymbol $ funcHeader func) func

        when ((sym $ funcSymbol $ funcHeader func) == "instance_main") $ do
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

    S.Let _ pattern Nothing Nothing -> do
        void $ generatePatternIsolated pattern

    S.Data _ symbol typ Nothing -> do
        base <- baseTypeOf typ
        init <- case base of
            TypeApply (Sym ["Tuple"]) [] -> return (C.Initialiser [])
            _                            -> return (C.Initialiser [C.Int 0])
        
        ctyp <- cTypeOf typ
        appendAssign ctyp (showSymLocal symbol) init
        define (showSymLocal symbol) $ Value typ $ C.Ident (showSymLocal symbol)

    S.If _ expr blk melse -> do
        val <- generateExpr expr
        if_ val $ generateStmt blk
        when (isJust melse) $ do
            elseID <- appendElem $ C.Else { elseStmts = [] }
            withCurID elseID $ generateStmt (fromJust melse)

    S.Switch _ cnd cases -> do
        newVal <- assign "switchExpr" =<< generateExpr cnd
        id <- appendElem $ C.Switch { switchBody = [], switchExpr = C.Int 0 }
        withCurID id $ do

            forM_ (zip cases [0..]) $ \((pattern, stmt), i) -> do
                caseId <- appendElem $ C.Case { caseExpr = C.Int i, caseBody = [] }
                withCurID caseId $ do
                    cnd <- generatePattern pattern newVal
                    if_ cnd $ do
                        generateStmt stmt
                        appendElem C.Break

            call "assert" [false]

    S.While _ expr stmt -> do
        id <- appendElem $ C.For Nothing Nothing Nothing []
        withCurID id $ do
            val <- generateExpr expr
            if_ (not_ val) $ appendElem C.Break
            generateStmt stmt
        
    S.For _ expr mpat stmt -> do
        idx <- assign "idx" (i64 0)
        first <- assign "first" true

        id <- appendElem $ C.For Nothing Nothing (Just $ C.Increment (valExpr idx)) []
        withCurID id $ do
            val <- generateExpr expr
            base <- baseTypeOf val
            -- special preable for ranges
            case base of
                TypeApply (Sym ["Table"]) _ -> return ()
                TypeApply (Sym ["Array"]) _ -> return ()
                TypeApply (Sym ["Tuple"]) [_, _] -> do
                    return ()
                    -- TODO

                Slice t -> return ()
                x -> error (show x)


            -- check that index is still in range
            idxGtEq <- case base of
                TypeApply (Sym ["Table"]) _  -> greaterEqual idx =<< builtinLen val
                TypeApply (Sym ["Array"]) [_, Size n]  -> greaterEqual idx (i64 n)
                TypeApply (Sym ["Tuple"]) [_, _] -> greaterEqual idx =<< member 1 val
                Slice t -> greaterEqual idx =<< builtinLen val
                x -> error (show x)
            if_ idxGtEq (appendElem C.Break)

            -- check that pattern matches
            patMatches <- case mpat of
                Nothing -> return true
                Just pat -> case base of
                    TypeApply (Sym ["Table"]) ts -> generatePattern pat =<< builtinTableAt val idx
                    TypeApply (Sym ["Array"]) ts -> generatePattern pat =<< builtinArrayAt val idx
                    TypeApply (Sym ["Tuple"]) ts -> generatePattern pat idx
                    Slice t -> generatePattern pat =<< builtinSliceAt val idx
                    _ -> error (show base)

            if_ (not_ patMatches) $ appendElem C.Break
            generateStmt stmt

    _ -> error "invalid statement"


generatePatternIsolated :: Pattern -> Generate Value
generatePatternIsolated (PatAnnotated pattern patType) = withPos pattern $ case pattern of
    PatIdent _ symbol -> do
        base <- baseTypeOf patType
        let name = showSymLocal symbol
        define name (Value patType $ C.Ident name)
        cType <- cTypeOf patType
        void $ appendAssign cType (showSymLocal symbol) $ C.Initialiser [C.Int 0]
        return true

    x -> error (show x)



generatePattern :: Pattern -> Value -> Generate Value
generatePattern (PatAnnotated pattern patType) val = withPos pattern $ case pattern of
    PatIgnore _ -> return true

    PatLiteral expr -> do
        v <- generateExpr expr
        callFunction (Sym ["Compare", "equal"]) Type.Bool [v, val]

    PatIdent _ symbol -> do 
        base <- baseTypeOf val
        let name = showSymLocal symbol
        define name (Value (typeof val) $ C.Ident name)
        cType <- cTypeOf (typeof val)
        void $ appendAssign cType (showSymLocal symbol) $ C.Initialiser [C.Int 0]
        ref <- reference $ Value (typeof val) (C.Ident name)
        callFunction (Sym ["Store", "store"]) Void [ref, val]
        return true

    PatSlice _ pats -> do
        ref <- reference val
        len <- callFunction (Sym ["Len", "len"]) I64 [ref] 
        endName <- fresh "end"

        lenMatch <- assign "lenMatch" $ Value Type.Bool $
            C.Infix C.EqEq (C.Int $ fromIntegral $ length pats) (valExpr len)

        match <- assign "match" false
        if_ (not_ lenMatch) (appendElem $ C.Goto endName)

        forM_ (zip pats [0..]) $ \(pat, i) -> do
            at <- callFunction (Sym ["At", "at"]) (typeof $ pats !! i) [val, i64 i]
            mat <- generatePattern pat at
            if_ (not_ mat) (appendElem $ C.Goto endName)

        store match true
        appendElem $ C.Label endName

        return match


    PatTuple _ pats -> do
        endLabel <- fresh "endPatTuple"
        match <- assign "match" false

        let symList = ["first", "second", "third", "fourth"]

        forM_ (zip pats [0..]) $ \(pat, i) -> do
            v <- callFunction (Sym [symList !! i]) (typeof pat) [val]
            b <- generatePattern pat v
            void $ if_ (not_ b) $ appendElem (C.Goto endLabel)

        store match true
        appendElem (C.Label endLabel)
        return match

    PatGuarded _ pat expr -> do
        match <- assign "match" =<< generatePattern pat val
        endLabel <- fresh "end"
        if_ (not_ match) $ appendElem (C.Goto endLabel)
        store match =<< generateExpr expr
        appendElem (C.Label endLabel)
        return match

    PatTypeField pos fieldType pats -> do
        TypeApply (Sym ["Sum"]) ts <- baseTypeOf val
        let Just i = elemIndex fieldType ts

        endLabel <- fresh "skipMatch"
        match <- assign "match" =<< (Value Type.Bool . C.Infix C.EqEq (C.Int $ fromIntegral i) . valExpr <$> adtEnum val)

        if_ (not_ match) $ appendElem $ C.Goto endLabel
        store match false

        case pats of
            [] -> return ()
            [pat] -> do
                patMatch <- generatePattern pat =<< member i val
                if_ (not_ patMatch) $ void $ appendElem (C.Goto endLabel)
            pats -> do
                let symList = ["first", "second", "third", "fourth"]
                forM_ (zip pats [0..]) $ \(pat, j) -> do
                    f <- member i val
                    v <- callFunction (Sym [symList !! j]) (typeof pat) [f]
                    b <- generatePattern pat v
                    void $ if_ (not_ b) $ appendElem (C.Goto endLabel)

        store match true
        appendElem $ C.Label endLabel
        return match

    _ -> error (show pattern)
generatePattern x _ = error (show x)


-- generateExpr should return a 're-enter-able' expression, eg 1, not func()
generateExpr :: Expr -> Generate Value
generateExpr (AExpr typ expr_) = withPos expr_ $ withTypeCheck $ case expr_ of
    S.Bool _ b             -> return $ Value typ (C.Bool b)
    S.Int _ n              -> return $ Value typ (C.Int n)
    S.Float _ f            -> return $ Value typ (C.Float f)
    S.Char _ c             -> return $ Value typ (C.Char c)
    S.Match _ expr pattern -> generatePattern pattern =<< generateExpr expr
    S.Ident _ symbol       -> look (showSymLocal symbol)
    S.String _ s           -> assign "string" $ Value typ $
        C.Initialiser [C.String s, C.Int (fromIntegral $ length s)]

    S.Builtin _ "conv" [expr] -> convert typ =<< generateExpr expr

    S.Builtin _ "builtin_zero" exprs -> do
        check (length exprs == 0) "builtin_zero cannot have arguments"
        base <- baseTypeOf typ
        name <- fresh "zero"
        ctyp <- cTypeOf typ
        appendElem $ C.Assign ctyp name $ C.Initialiser $ case base of
            TypeApply (Sym ["Tuple"]) [] -> []
            _                          -> [C.Int 0]
        return $ Value typ (C.Ident name)

    S.Builtin _ "builtin_pretend" [expr] -> do
        Ref exprType refExpr <- generateExpr expr

        base <- baseTypeOf typ
        baseExpr <- baseTypeOf exprType
        check (base == baseExpr) "types do not have same base"

        case base of
            TypeApply (Sym ["Tuple"]) _ ->
                assign "ref" $ Ref typ $ C.Initialiser [C.Member refExpr "ptr"]
            _ -> 
                assign "ref" $ Ref typ $ C.Initialiser [refExpr]


    S.Builtin _ "builtin_table_slice" [expr, start, end] -> do
        ref@(Ref _ exp) <- generateExpr expr
        srt@(Value I64 _) <- generateExpr start
        en@(Value I64 _) <- generateExpr end

        -- TODO this is broken 
        TypeApply (Sym ["Table"]) [t] <- baseTypeOf ref
        assign "slice" $ Value (Slice t) $
            C.Initialiser
                [ C.Address (C.Subscript (C.PMember exp "r0") (valExpr srt))
                , C.Infix C.Minus (C.PMember exp "len") (valExpr srt)
                ] 


    S.Builtin _ "builtin_table_at" [expr1, expr2] -> do
        val <- generateExpr expr1
        idx <- generateExpr expr2
        TypeApply (Sym ["Table"]) _ <- baseTypeOf val
        builtinTableAt val idx

    S.Builtin _ "builtin_slice_at" [expr1, expr2] -> do
        val <- generateExpr expr1
        idx <- generateExpr expr2
        Slice _ <- baseTypeOf val
        builtinSliceAt val idx

    S.Builtin _ "builtin_array_at" [expr1, expr2] -> do
        val <- generateExpr expr1
        idx <- generateExpr expr2
        TypeApply (Sym ["Array"]) [t, Size n] <- baseTypeOf val
        builtinArrayAt val idx

    S.Builtin _ "builtin_table_append" [expr1] -> do
        val <- generateExpr expr1
        TypeApply (Sym ["Table"]) _ <- baseTypeOf val
        case val of
            Value _ _ -> fail "isn't reference"
            Ref _ _   -> builtinTableAppend val >> return (Value Void $ C.Int 0)

    S.Call _ symbol exprs -> do
        check (symbolIsResolved symbol) ("unresolved function call: " ++ prettySymbol symbol)
        callFunction symbol typ =<< mapM generateExpr exprs

    S.Field _ expr idx -> do 
        member idx =<< generateExpr expr

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
