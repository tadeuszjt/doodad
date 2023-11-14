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

generate :: ASTResolved -> Generate ()
generate ast = withErrorPrefix "generate: " $ do
    -- copy members from resolved ast
    modify $ \s -> s { ctors = ctorDefs ast }
    modify $ \s -> s { typefuncs = typeFuncs ast } 

    forM_ (Map.toList $ constDefs ast) $ \(symbol, expr) -> do
        define (show symbol) (CGenerate.Const expr)
            
    -- generate imported function externs
    forM_ (Map.toList $ funcImports ast) $ \(symbol, body) -> do
        unless (isGenericBody body) $ do
            crt <- cTypeOf (ASTResolved.funcRetty body)
            cpts <- map Cpointer <$> mapM cTypeOf (ASTResolved.funcParams body)
            cats <- mapM cTypeOf (ASTResolved.funcArgs body)
            newExtern (show symbol) crt (cpts ++ cats)

    -- generate function headers
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
         unless (isGenericBody func) $ do
            crt <- cTypeOf (ASTResolved.funcRetty func)
            cpts <- map Cpointer <$> mapM cTypeOf (map paramType $ ASTResolved.funcParams func)
            cats <- mapM cTypeOf (map paramType $ ASTResolved.funcArgs func)
            newExtern (show symbol) crt (cpts ++ cats)

    -- generate functions, main is a special case
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
        unless (isGenericBody func) $ do
            generateFunc symbol func
            when (sym symbol == "main") $ do
--                let typedef = Type.TypeApply (SymResolved "io" "Io" 0) []
                id <- newFunction Cint "main" [C.Param "argc" Cint, C.Param "argv" (Cpointer (Cpointer Cchar))]
                withCurID id $ case (ASTResolved.funcParams func, ASTResolved.funcArgs func) of
                    ([], []) -> do
                        appendElem $ C.ExprStmt $ C.Call "doodad_set_args" [C.Ident "argc", C.Ident "argv"]
                        call (show symbol) []
                        appendElem $ C.Return $ C.Int 0
--                    ([p], []) | typeof p == typedef -> do -- main with io
--                        io <- initialiser typedef []
--                        callWithParams [io] (show symbol) []
                withCurID globalID (append id)


generateFunc :: Symbol -> FuncBody -> Generate ()
generateFunc symbol body = do
    args <- mapM cParamOf (ASTResolved.funcArgs body)
    params <- map (\(C.Param n t) -> C.Param n (Cpointer t)) <$> mapM cParamOf (ASTResolved.funcParams body)
    rettyType <- cTypeOf (ASTResolved.funcRetty body)

    pushSymTab
    forM_ (ASTResolved.funcParams body) $ \param -> do
        ctyp <- cTypeOf (paramType param)
        name <- return $ show (S.paramName param)
        define name $ Value (S.paramType param) $ C.Deref (C.Ident name)

    forM_ (ASTResolved.funcArgs body) $ \arg -> do
        ctyp <- cTypeOf (paramType arg)
        name <- return $ show (S.paramName arg)
        define name $ Value (S.paramType arg) (C.Ident name)

    id <- newFunction rettyType (show symbol) (params ++ args)
    withCurID id $ do
        generateStmt (ASTResolved.funcStmt body)
        when (ASTResolved.funcRetty body /= Type.Void) $ -- check to ensure function has return
            call "assert" [false]
    popSymTab
    withCurID globalID $ append id


generatePrint :: String -> Value -> Generate ()
generatePrint app val = case typeof val of
    Type.I64 ->    void $ appendPrintf ("%d" ++ app) [valExpr val]
    Type.I32 ->    void $ appendPrintf ("%d" ++ app) [valExpr val]
    Type.F64 ->    void $ appendPrintf ("%f" ++ app) [valExpr val]
    Type.F32 ->    void $ appendPrintf ("%f" ++ app) [valExpr val]
    Type.String -> void $ appendPrintf ("\"%s\"" ++ app) [valExpr val]
    Type.Char ->   void $ appendPrintf ("%c" ++ app) [valExpr val]

    Type.Bool -> void $ appendPrintf ("%s" ++ app) $
        [C.CndExpr (valExpr val) (C.String "true") (C.String "false")]

    Type.Tuple t -> do
        baseT <- baseTypeOf t
        case baseT of
            Record _ -> do
                ts <- getRecordTypes t
                call "putchar" [Value Type.Char $ C.Char '(']
                forM_ (zip ts [0..]) $ \(t, i) -> do
                    let end = i == length ts - 1
                    let member = Value t $ C.Member (valExpr val) ("m" ++ show i)
                    generatePrint (if end then (")" ++ app) else ", ") member
            _ -> generatePrint app $ Value t (valExpr val)

    Type.TypeApply s t -> do
        base <- baseTypeOf val
        generatePrint app $ Value base (valExpr val)

    Type.Record ts -> do
        call "putchar" [Value Type.Char $ C.Char '{']
        forM_ (zip ts [0..]) $ \(t, i) -> do
            let end = i == length ts - 1
            generatePrint (if end then "" else ", ") =<< member i val
        void $ appendPrintf ("}" ++ app) []

    Type.ADT ts -> do -- TODO
        void $ appendPrintf ("ADT" ++ app) []

    _ -> error (show $ typeof val)


generateIndex :: S.Expr -> Generate Value
generateIndex expr_@(S.AExpr t expr__) = case expr__ of
    S.Ident _ _    -> generateExpr expr_
    S.Field _ _ _  -> generateExpr expr_
    S.Call _ _ _ _ -> generateExpr expr_

    S.Subscript _ expr idx -> do
        val <- generateIndex expr
        base <- baseTypeOf expr
        case base of
            Type.String -> fail "cannot index string"
            Type.Table typ -> accessRecord val =<< fmap Just (generateExpr idx)
            _ -> error (show base)

    S.RecordAccess _ expr -> do
        val <- generateExpr expr
        accessRecord val Nothing

    _ -> error (show expr__)
generateIndex e = error (show e)


generateStmt :: S.Stmt -> Generate ()
generateStmt stmt = withPos stmt $ case stmt of
    S.EmbedC _ str -> void $ appendElem (C.Embed str)
    S.Block stmts -> mapM_ generateStmt stmts
    S.Return _ Nothing -> void $ appendElem (C.ReturnVoid)
    S.Return _ (Just expr) -> void $ appendElem . C.Return . valExpr =<< generateExpr expr
    S.Const _ symbol expr -> do
        define (show symbol) $ CGenerate.Const expr

    S.Increment _ expr -> do
        val <- generateExpr expr
        base <- baseTypeOf expr
        case base of
            Table t -> tableAppend val

    S.Let _ pattern expr mblk -> do
        matched <- generatePattern pattern =<< generateExpr expr
        call "assert" [matched]
        case mblk of
            Nothing -> return ()
            Just blk -> generateStmt blk

    S.Data _ symbol typ Nothing -> do
        ctyp <- cTypeOf typ
        appendAssign ctyp (show symbol) (C.Initialiser [C.Int 0])
        define (show symbol) $ Value typ $ C.Ident (show symbol)

    S.If _ expr blk melse -> do
        val <- generateExpr expr
        if_ val $ generateStmt blk
        when (isJust melse) $ do
            elseID <- appendElem $ C.Else { elseStmts = [] }
            withCurID elseID $ generateStmt (fromJust melse)

    S.Switch _ cnd cases -> do
        newVal <- assign "switchExpr" =<< generateExpr cnd
        withFakeSwitch $ do
            forM_ cases $ \(pattern, stmt) -> do
                cnd <- generatePattern pattern newVal
                if_ cnd $ do
                    generateStmt stmt
                    appendElem C.Break
            call "assert" [false]

    S.ExprStmt (AExpr _ (S.Call _ exprs1 symbol exprs2)) -> do
        check (symbolIsResolved symbol) ("unresolved function call: " ++ show symbol)
        params <- mapM generateExpr exprs1
        args <- mapM generateExpr exprs2
        callWithParams params (show symbol) args

    S.ExprStmt (AExpr _ (S.Builtin _ "print" exprs)) -> do
        vals <- mapM generateExpr exprs
        forM_ (zip vals [0..]) $ \(val, i) -> do
            let end = i == length vals - 1
            generatePrint (if end then "\n" else ", ") val

    S.ExprStmt (AExpr _ (S.Builtin pos "assert" [cnd, str])) -> do
        cndVal <- generateExpr cnd
        strVal <- generateExpr str
        fileNameVal <- return $ Value Type.String (C.String $ textFile pos)
        lineVal <- return $ Value I64 (C.Int $ fromIntegral $ textLine pos)
        call "doodad_assert" [fileNameVal, lineVal, cndVal, strVal]

    S.SetOp _ S.Eq index expr -> do
        idx <- generateIndex index
        set idx =<< generateExpr expr

    S.While _ expr stmt -> do
        id <- appendElem $ C.For Nothing Nothing Nothing []
        withCurID id $ do
            val <- generateExpr expr
            if_ (not_ val) $ appendElem C.Break
            generateStmt stmt
        
    S.For _ expr mpat stmt -> do
        base <- baseTypeOf expr
        idx <- assign "idx" (i64 0)
        first <- assign "first" true

        id <- appendElem $ C.For Nothing Nothing (Just $ C.Increment (valExpr idx)) []
        withCurID id $ do
            val <- generateExpr expr
            -- special preable for ranges
            case base of
                Type.String -> return ()
                Type.Table _ -> return ()
                Type.Range I64 -> do
                    if_ first $ do
                        set idx $ Value I64 (C.Member (valExpr val) "min")
                        set first false
                        return ()

            -- check that index is still in range
            idxGtEq <- case base of
                Type.Range I64 -> generateInfix S.GTEq idx $ Value I64 (C.Member (valExpr val) "max")
                Type.String    -> generateInfix S.GTEq idx =<< len val
                Type.Table _  -> generateInfix S.GTEq idx =<< len val
            if_ idxGtEq (appendElem C.Break)

            -- check that pattern matches
            patMatches <- case mpat of
                Nothing -> return true
                Just pat -> case base of
                    Type.Table ts -> generatePattern pat =<< accessRecord val (Just idx)
                    Type.Range t  -> generatePattern pat idx
                    _ -> error (show base)

            if_ (not_ patMatches) $ appendElem C.Break
            generateStmt stmt

    _ -> fail (show stmt)


-- creates an expression which may be used multiple times without side-effects
generateReentrantExpr :: Value -> Generate Value
generateReentrantExpr (CGenerate.Const expr) = return (CGenerate.Const expr)
generateReentrantExpr (Value typ expr) = Value typ <$> reentrantExpr expr
    where
        reentrantExpr :: C.Expression -> Generate C.Expression
        reentrantExpr expr = case expr of
            C.Ident _ -> return expr
            C.Bool _  -> return expr
            C.String _ -> return expr
            C.Int _ -> return expr
            C.Float _ -> return expr
            C.Char _ -> return expr
            C.Not e -> C.Not <$> reentrantExpr e
            C.Deref e -> C.Deref <$> reentrantExpr e
            C.Address e -> C.Address <$> reentrantExpr e
            C.Cast t e -> C.Cast t <$> reentrantExpr e
            C.Prefix op e -> C.Prefix op <$> reentrantExpr e
            C.Infix op e1 e2 -> do
                e1' <- reentrantExpr e1
                e2' <- reentrantExpr e2
                return $ C.Infix op e1' e2'
            C.Subscript e1 e2 -> do
                e1' <- reentrantExpr e1
                e2' <- reentrantExpr e2
                return $ C.Subscript e1' e2'
            C.Member e1 str -> do
                e1' <- reentrantExpr e1
                return $ C.Member e1' str
            C.Call str es -> do
                fmap valExpr $ assign "call" . Value typ =<< C.Call str <$> mapM reentrantExpr es
            C.Initialiser elems -> do
                fmap valExpr $ assign "init" . Value typ =<< C.Initialiser <$> mapM reentrantExpr elems
            _ -> error (show expr)


--  -> {x}        // x works like... a normal ident
--  -> {(x, y)}   // x and y are not references?
--  -> {x, y}     // x and y work like idents
--  -> ({x}, {y}) // yeah, x and y are idents
--  Maybe( {}i64 ) -> Just( x )
generatePattern :: Pattern -> Value -> Generate Value
generatePattern pattern val = withPos pattern $ do
    case pattern of
        PatIgnore _ -> return true
        PatLiteral expr -> generateInfix S.EqEq val =<< generateExpr expr
        PatAnnotated pat typ -> generatePattern pat val

        PatRecord _ pats -> do
            base <- baseTypeOf val
            case base of
                Record ts -> do -- TODO patterns aren't references
                    unless (length ts == length pats) (error "invalid record length")
                    endLabel <- fresh "end"
                    match <- assign "match" false
                    forM_ (zip3 pats [0..] ts) $ \(pat, i, t) -> do
                        b <- case pat of
                            PatAnnotated (PatIdent _ symbol) _ -> do
                                define (show symbol) =<< member i val
                                return true
                            _ -> generatePattern pat =<< member i val
                        if_ (not_ b) $ appendElem (C.Goto endLabel)

                    set match true
                    appendElem $ C.Label endLabel
                    return match
                        
                _ -> error (show base)

        PatIdent _ symbol -> do 
            let name = show symbol
            define name (Value (typeof val) $ C.Ident name)
            cType <- cTypeOf (typeof val)
            void $ appendAssign cType (show symbol) $ C.Initialiser [C.Int 0]
            set (Value (typeof val) $ C.Ident name) val
            return true

        PatTuple _ pats -> do
            base@(Type.Tuple t) <- baseTypeOf val
            baseT <- baseTypeOf t

            case baseT of
                Record _ -> do
                    ts <- getRecordTypes t
                    unless (length pats == length ts) (error "invalid tuple length")
                    endLabel <- fresh "end"
                    match <- assign "match" false

                    forM_ (zip3 pats ts [0..]) $ \(pat, t, i) -> do
                        let member = Value t $ C.Member (valExpr val) ("m" ++ show i)
                        b <- generatePattern pat member
                        if_ (not_ b) $ appendElem (C.Goto endLabel)
                                
                    set match true
                    appendElem (C.Label endLabel)
                    return match

        PatGuarded _ pat expr -> do
            match <- assign "match" =<< generatePattern pat val
            endLabel <- fresh "end"
            if_ (not_ match) $ appendElem (C.Goto endLabel)
            set match =<< generateExpr expr
            appendElem (C.Label endLabel)
            return match

        PatField pos symbol pats -> do
            ADT ts <- baseTypeOf val
            (s, i) <- mapGet symbol =<< gets ctors

            endLabel <- fresh "skipMatch"
            let TypeApply typeSymbol _ = typeof val
            unless (s == typeSymbol) (error "type mismatch")

            match <- assign "match" =<< generateInfix S.EqEq (i64 i) =<< adtEnum val
            if_ (not_ match) $ appendElem $ C.Goto endLabel
            set match false

            case pats of
                [] -> return ()
                [pat] -> do
                    patMatch <- generatePattern pat =<< member i val
                    if_ (not_ patMatch) $ void $ appendElem (C.Goto endLabel)
                pats -> do
                    -- Bit of a hack to use PatTuple
                    patMatch <- generatePattern (PatTuple pos pats) =<< member i val
                    if_ (not_ patMatch) $ void $ appendElem (C.Goto endLabel)

            set match true
            appendElem $ C.Label endLabel
            return match

        _ -> error (show pattern)


annotateExprWith :: Type.Type -> S.Expr -> Generate S.Expr
annotateExprWith typ expr = do
    base <- baseTypeOf typ
    fmap (AExpr typ) $ case expr of
        S.Int _ _ -> return expr
--        S.Array pos es -> case base of
--                Type.Table [t] -> S.Array pos <$> mapM (annotateExprWith t) es
--        S.Tuple pos es -> case base of
--            Type.Tuple ts -> S.Tuple pos <$> zipWithM annotateExprWith ts es
        
        _ -> error (show expr)


-- generateExpr should return a 're-enter-able' expression, eg 1, not func()
generateExpr :: Expr -> Generate Value
generateExpr (AExpr typ expr_) = withPos expr_ $ withTypeCheck $ case expr_ of
    S.Bool _ b   -> return $ Value typ (C.Bool b)
    S.Int _ n    -> return $ Value typ (C.Int n)
    S.Float _ f  -> return $ Value typ (C.Float f)
    S.String _ s -> return $ Value typ (C.String s)
    S.Char _ c   -> return $ Value typ (C.Char c)
    S.Match _ expr pattern -> generatePattern pattern =<< generateExpr expr

    S.Builtin _ "conv" [expr] -> convert typ =<< generateExpr expr

    S.Builtin _ "len" exprs -> do 
        unless (length exprs == 1) (error "invalid len call")
        len =<< generateExpr (head exprs)

    S.Ident _ symbol -> do
        obj <- look (show symbol)
        case obj of
            Value _ _ -> return obj
            CGenerate.Const e -> generateExpr =<< annotateExprWith typ e

    S.RecordAccess _ expr -> do
        val <- generateExpr expr
        base <- baseTypeOf val
        accessRecord val Nothing

    S.Prefix _ op a -> do
        val <- generateExpr a
        base <- baseTypeOf val
        case base of
            Type.Bool -> case op of
                S.Not -> return (not_ val)
            Type.I64 -> case op of
                S.Minus -> generateInfix S.Minus (i64 0) val
            Type.F32 -> case op of
                S.Minus -> return $ Value (typeof val) $ C.Prefix C.Minus (valExpr val)
            _ -> error (show base) 

    S.Infix _ op a b -> do
        valA <- generateExpr a
        valB <- generateExpr b
        generateInfix op valA valB

    S.Call _ exprs1 symbol exprs2 -> do
        check (symbolIsResolved symbol) ("unresolved function call: " ++ show symbol)
        objs1 <- mapM generateExpr exprs1
        objs2 <- mapM generateExpr exprs2
        return $ Value typ $ C.Call (show symbol) (map ptrExpr objs1 ++ map valExpr objs2)

    S.Field _ _ (Sym s) -> fail $ "unresolved field: " ++ s
    S.Field _ expr symbol -> do 
        resm <- Map.lookup symbol <$> gets ctors
        index <- case resm of
            Just (typeSymbol, i) -> return i
            Nothing              -> getTypeFieldIndex (typeof expr) (TypeApply symbol [])

            _ -> error (show resm)

        --assert (typ == typeof val) "ctor type mismatch" TODO
        member index =<< generateExpr expr

    S.Tuple _ exprs -> do
        vals <- mapM generateExpr exprs
        base <- baseTypeOf typ
        case base of
            Type.Tuple t -> initialiser typ vals -- TODO
            _ -> error (show base)

    S.Range _ (Just expr) mexpr1 mexpr2 -> do
        val <- generateExpr expr
        base <- baseTypeOf val
        start <- case base of
            Type.Table _ -> case mexpr1 of
                Nothing -> return (i64 0)
            Type.String -> case mexpr1 of
                Nothing -> return (i64 0)
            _ -> error (show base)
        end <- case base of
            Type.Table _ -> case mexpr2 of
                Nothing -> len val
            Type.String -> case mexpr1 of
                Nothing -> len val
            _ -> error (show base)
        initialiser typ [start, end]

    S.Range _ Nothing (Just expr1) (Just expr2) -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        unless (typeof val1 == typeof val2) (error "type mismatch")
        initialiser typ [val1, val2]

    S.Construct pos symbol exprs -> do
        vals <- mapM generateExpr exprs
        (typeSymbol, i) <- mapGet symbol =<< gets ctors
        base <- baseTypeOf typ
        case base of
            Type.ADT ts -> do
                unless (i < length ts) (error "invalid index")
                adt <- assign "adt" $ Value typ (C.Initialiser [C.Int $ fromIntegral i])
                let fieldType = ts !! i
                case vals of
                    []    -> unless (fieldType == Void) (error "null field cannot have arguments")
                    [val] -> set (Value fieldType $ C.Member (valExpr adt) ("u" ++ show i)) val
                    vals  -> do
                        tup <- initialiser fieldType vals
                        set (Value fieldType $ C.Member (valExpr adt) ("u" ++ show i)) tup

                return adt

            _ -> error (show base)

    S.Subscript _ expr arg -> do
        val <- generateExpr expr
        argVal <- generateExpr arg
        base <- baseTypeOf val
        case base of
            Type.String -> return $ Value typ $ C.Subscript (valExpr val) (valExpr argVal)
            _ -> accessRecord val . Just =<< generateExpr arg


    _ -> error (show expr_)
    where
        withTypeCheck :: Generate Value -> Generate Value
        withTypeCheck f = do
            r <- generateReentrantExpr =<< f
            unless (typeof r == typ) (error "generateExpr returned incorrect type")
            return r
generateExpr x = fail $ "unresolved expression: " ++ show x
            

generateInfix :: S.Operator -> Value -> Value -> Generate Value
generateInfix op a b = do
    unless (typeof a == typeof b) (error "type mismatch")
    base <- baseTypeOf a
    case base of
        _ | base `elem` [Type.I8, Type.I16, Type.I32, Type.I64, Type.U8, Type.F32, Type.F64] ->
            return $ case op of
                S.Plus ->   Value (typeof a) $ C.Infix C.Plus (valExpr a) (valExpr b) 
                S.Times ->  Value (typeof a) $ C.Infix C.Times (valExpr a) (valExpr b) 
                S.Minus ->  Value (typeof a) $ C.Infix C.Minus (valExpr a) (valExpr b)
                S.Modulo -> Value (typeof a) $ C.Infix C.Modulo (valExpr a) (valExpr b)
                S.Divide -> Value (typeof a) $ C.Infix C.Divide (valExpr a) (valExpr b)
                S.LT ->     Value Type.Bool  $ C.Infix C.LT (valExpr a) (valExpr b)
                S.GT ->     Value Type.Bool  $ C.Infix C.GT (valExpr a) (valExpr b)
                S.LTEq ->   Value Type.Bool  $ C.Infix C.LTEq (valExpr a) (valExpr b)
                S.EqEq ->   Value Type.Bool  $ C.Infix C.EqEq (valExpr a) (valExpr b)
                S.GTEq ->   Value Type.Bool  $ C.Infix C.GTEq (valExpr a) (valExpr b)
                S.NotEq ->  Value Type.Bool  $ C.Infix C.NotEq (valExpr a) (valExpr b)
                _ -> error (show op)

        Type.Bool -> return $ case op of
            S.AndAnd -> Value (typeof a) $ C.Infix C.AndAnd (valExpr a) (valExpr b)
            S.OrOr   -> Value (typeof a) $ C.Infix C.OrOr (valExpr a) (valExpr b)
            S.EqEq   -> Value (typeof a) $ C.Infix C.EqEq (valExpr a) (valExpr b)
            S.LT     -> Value (typeof a) $ C.Infix C.LT (valExpr a) (valExpr b)
            S.GT     -> Value (typeof a) $ C.Infix C.GT (valExpr a) (valExpr b)
            _ -> error (show op)

        Type.Char -> return $ case op of
            S.Minus -> Value (typeof a) $ C.Infix C.Minus (valExpr a) (valExpr b)
            S.EqEq  -> Value Type.Bool $ C.Infix (C.EqEq) (valExpr a) (valExpr b)
            S.NotEq -> Value Type.Bool $ C.Infix (C.NotEq) (valExpr a) (valExpr b)
            o -> error (show o)

        Type.String -> case op of
            S.Plus -> return $ Value (typeof a) (C.Call "doodad_string_plus" [valExpr a, valExpr b])
            S.EqEq -> return $ Value Type.Bool  (C.Call "doodad_string_eqeq" [valExpr a, valExpr b])
            S.NotEq -> return $ Value Type.Bool $ C.Not (C.Call "doodad_string_eqeq" [valExpr a, valExpr b]) 
            S.LT   -> return $ Value Type.Bool  (C.Call "doodad_string_lt"   [valExpr a, valExpr b])
            S.GT   -> return $ Value Type.Bool  (C.Call "doodad_string_gt"   [valExpr a, valExpr b])
            _ -> error (show op)

        Type.Record xs -> do
            ts <- getRecordTypes (Type.Record xs)
            res <- assign "res" true
            withFakeSwitch $ do
                forM_ (zip ts [0..]) $ \(t, i) -> do
                    let da = Value t $ C.Deref $ C.Member (valExpr a) ("m" ++ show i) 
                    let db = Value t $ C.Deref $ C.Member (valExpr b) ("m" ++ show i) 
                    case op of
                        _ | op `elem` [S.EqEq, S.LTEq, S.GTEq] -> do 
                            b <- generateInfix op da db
                            if_ (not_ b) $ do
                                set res false
                                appendElem C.Break

                        S.NotEq -> do
                            neq <- generateInfix S.NotEq da db
                            if_ (neq) (appendElem C.Break)

                        S.LT -> do
                            lt <- generateInfix S.LT da db
                            if_ lt (appendElem C.Break)
                            eq <- generateInfix S.EqEq da db
                            if_ (not_ eq) $ do
                                set res false
                                appendElem C.Break

                        S.GT -> do
                            gt <- generateInfix S.GT da db
                            if_ gt (appendElem C.Break)
                            eq <- generateInfix S.EqEq da db
                            if_ (not_ eq) $ do
                                set res false
                                appendElem C.Break

                case op of
                    S.NotEq -> set res false
                    S.LT    -> set res false
                    S.GT    -> set res false
                    _       -> return ()

            return res

        Type.Tuple t -> do
            ts <- getRecordTypes =<< baseTypeOf t
            res <- assign "res" true
            withFakeSwitch $ do
                forM_ (zip ts [0..]) $ \(t, i) -> do
                    let ma = Value t $ C.Member (valExpr a) ("m" ++ show i)
                    let mb = Value t $ C.Member (valExpr b) ("m" ++ show i)
                    eq <- generateInfix S.EqEq ma mb
                    case op of
                        S.NotEq -> if_ (not_ eq) (appendElem C.Break)
                        S.EqEq -> if_ (not_ eq) $ do
                            set res false
                            appendElem C.Break

                        S.LT -> do
                            lt <- generateInfix S.LT ma mb
                            if_ lt (appendElem C.Break)
                            if_ (not_ eq) $ do
                                set res false
                                appendElem C.Break

                        S.GT -> do
                            gt <- generateInfix S.GT ma mb
                            if_ gt (appendElem C.Break)
                            if_ (not_ eq) $ do
                                set res false
                                appendElem C.Break

                case op of
                    S.NotEq -> set res false
                    S.LT    -> set res false
                    S.GT    -> set res false
                    _       -> return ()

            return res
            
        _ -> error $ show (base, op)
