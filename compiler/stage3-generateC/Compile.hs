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
import States
import Symbol
import Error



getSymbolsOrderedByDependencies :: Monad m => Map.Map Symbol Type.Type -> m [Symbol]
getSymbolsOrderedByDependencies typedefs = do
    fmap (removeDuplicates . concat) . forM (Map.toList typedefs) $ \(s, t) -> do
        symbols <- getSymbols t
        return (symbols ++ [s])
    where
        getSymbols :: Monad m => Type.Type -> m [Symbol]
        getSymbols typ = case typ of
            x | isSimple x -> return []
            Type.Typedef s -> do
                symbols <- getSymbols (typedefs Map.! s)
                return $ symbols ++ [s]
            Type.Tuple ts  -> concat <$> mapM getSymbols ts
            Table ts  -> concat <$> mapM getSymbols ts
            Sparse ts  -> concat <$> mapM getSymbols ts
            Type.Array n t -> getSymbols t
            Type.ADT fs -> concat <$> mapM getSymbolsField fs
            _ -> error (show typ)

        getSymbolsField :: Monad m => AdtField -> m [Symbol]
        getSymbolsField field = case field of
            FieldNull -> return []
            FieldType t -> getSymbols t
            FieldCtor ts -> concat <$> mapM getSymbols ts
            _ -> error (show field)
        
        removeDuplicates :: Eq a => [a] -> [a]
        removeDuplicates [] = []
        removeDuplicates (x:xs)
            | elem x xs = x:removeDuplicates (deleteAll x xs)
            | otherwise = x:removeDuplicates xs

        deleteAll :: Eq a => a -> [a] -> [a]
        deleteAll a [] = []
        deleteAll a (x:xs)
            | a == x    = deleteAll a xs
            | otherwise = x : deleteAll a xs



generateAdtInit :: MonadGenerate m => Type.Type -> Value -> m Value
generateAdtInit typ val = do
    base@(Type.ADT fs) <- baseTypeOf typ
    let field = Type.FieldType (typeof val)
    assert (field `elem` fs) "TODO"
    let i = fromJust $ elemIndex field fs
    adt <- assign "adt" $ Value typ $ C.Initialiser [C.Int $ fromIntegral i]
    un <- member i adt
    set un val
    return adt

generateAdtEqual :: MonadGenerate m => Value -> Value -> m Value
generateAdtEqual a b = do
    assert (typeof a == typeof b) "types aren't equal"
    base@(Type.ADT fs) <- baseTypeOf a

    enA <- adtEnum a
    enB <- adtEnum b
    eq <- assign "adtEq" =<< generateInfix S.EqEq enA enB
    
    if_ eq $ do
        switchId <- appendElem $ C.Switch (valExpr enA) []
        withCurID switchId $ do
            forM_ (zip fs [0..]) $ \(field, i) -> case field of
                FieldNull -> return ()
                FieldCtor [] -> return ()
                -- set eq to false if we have a field to check

    return eq

generate :: MonadGenerate m => ResolvedAst -> m ()
generate ast = do
    let typedefs = Map.union (typeImports ast) (typeDefs ast)

    -- copy ctors
    modify $ \s -> s { ctors = Map.union (ctorDefs ast) (ctorImports ast) }
    modify $ \s -> s { typedefs = Map.union (typeDefs ast) (typeImports ast) }

    orderedSymbols <- getSymbolsOrderedByDependencies typedefs
    forM_ orderedSymbols $ \symbol -> do
        when (Map.member symbol typedefs) $ do
            ctype <- cTypeOf (typedefs Map.! symbol)
            void $ newTypedef ctype (show symbol)
            
    -- generate imported function externs
    forM_ (Map.toList $ funcImports ast) $ \(symbol, funcKey@(pts, s, ats, rt)) -> case symbol of
        SymResolved _ _ _ -> do
            crt <- cTypeOf rt
            cpts <- map Cpointer <$> mapM cTypeOf pts
            cats <- mapM cTypeOf ats
            newExtern (show symbol) crt (cpts ++ cats)
        _ -> return ()

    -- generate function headers
    
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
        crt <- cTypeOf (States.funcRetty func)
        cpts <- map Cpointer <$> mapM cTypeOf (map paramType $ States.funcParams func)
        cats <- mapM cTypeOf (map paramType $ States.funcArgs func)
        newExtern (show symbol) crt (cpts ++ cats)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
        generateFunc symbol func
        when (sym symbol == "main") $ do
            let typedef = Type.Typedef (SymResolved "io" "Io" 0)
            id <- newFunction Cvoid "main" []
            withCurID id $ case (States.funcParams func, States.funcArgs func) of
                ([], []) -> call (show symbol) []
                ([p], []) | typeof p == typedef -> do -- main with io
                    io <- assign "io" $ Value typedef (C.Initialiser [])
                    callWithParams [io] (show symbol) []
            withCurID globalID (append id)


generateFunc :: MonadGenerate m => Symbol -> FuncBody -> m ()
generateFunc symbol body = do
    args <- mapM cParamOf (States.funcArgs body)
    params <- map (\(C.Param n t) -> C.Param n (Cpointer t)) <$> mapM cParamOf (States.funcParams body)
    rettyType <- cTypeOf (States.funcRetty body)

    forM_ (States.funcParams body) $ \param -> do
        ctyp <- cTypeOf (paramType param)
        name <- return $ show (S.paramName param)
        define name $ Value (S.paramType param) (C.Deref $ C.Ident name)

    forM_ (States.funcArgs body) $ \arg -> do
        ctyp <- cTypeOf (paramType arg)
        name <- return $ show (S.paramName arg)
        define name $ Value (S.paramType arg) (C.Ident name)

    id <- newFunction rettyType (show symbol) (params ++ args)
    withCurID id $ do
        mapM_ generateStmt (States.funcStmts body)
        when (States.funcRetty body /= Type.Void) $ -- check to ensure function has return
            call "assert" [false]
    withCurID globalID $ append id



generatePrint :: MonadGenerate m => String -> Value -> m ()
generatePrint app val = case typeof val of
    Type.I64 ->    void $ appendPrintf ("%d" ++ app) [valExpr val]
    Type.F64 ->    void $ appendPrintf ("%f" ++ app) [valExpr val]
    Type.String -> void $ appendPrintf ("%s" ++ app) [valExpr val]
    Type.Char ->   void $ appendPrintf ("%c" ++ app) [valExpr val]

    Type.Bool -> void $ appendPrintf ("%s" ++ app) $
        [C.CndExpr (valExpr val) (C.String "true") (C.String "false")]

    Type.Tuple ts -> do
        call "putchar" [Value Type.Char $ C.Char '(']
        forM_ (zip ts [0..]) $ \(t, i) -> do
            let end = i == length ts - 1
            generatePrint (if end then "" else ", ") =<< member i val
        void $ appendPrintf (")" ++ app) []

    _ -> error (show $ typeof val)


generateStmt :: MonadGenerate m => S.Stmt -> m ()
generateStmt stmt = case stmt of
    S.EmbedC _ str -> void $ appendElem (C.Embed str)
    S.Block stmts -> mapM_ generateStmt stmts
    S.Return _ Nothing -> void $ appendElem (C.ReturnVoid)
    S.Return _ (Just expr) -> void $ appendElem . C.Return . valExpr =<< generateExpr expr
    S.FuncDef _ _ _ _ _ _ -> return ()

    S.Assign _ pattern expr -> do
        matched <- generatePattern pattern =<< generateExpr expr
        call "assert" [matched]

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
        switchId <- appendElem $ C.Switch { switchBody = [], switchExpr = C.Int 0 }
        caseId <- newElement $ C.Case { caseExpr = C.Int 0, caseBody = [] }
        withCurID switchId $ append caseId
        withCurID caseId $ do
            forM_ cases $ \(pattern, stmt) -> do
                cnd <- generatePattern pattern newVal
                if_ cnd $ do
                    generateStmt stmt
                    appendElem C.Break
            call "assert" [false]


    S.ExprStmt (AExpr _ (S.Call _ exprs1 symbol exprs2)) -> do
        params <- mapM generateExpr exprs1
        args <- mapM generateExpr exprs2
        callWithParams params (show symbol) args

    S.ExprStmt (AExpr _ (S.Builtin _ [] "print" exprs)) -> do
        vals <- mapM generateExpr exprs
        forM_ (zip vals [0..]) $ \(val, i) -> do
            let end = i == length vals - 1
            generatePrint (if end then "\n" else ", ") val

    S.SetOp _ S.Eq expr1 expr2 -> do
        val1 <- generateExpr expr1
        set val1 =<< generateExpr expr2


    S.SetOp _ op expr1 (S.AExpr t2 expr2) -> do
        val1 <- generateExpr expr1
        assert (typeof val1 == t2) "types do not match"
        base <- baseTypeOf val1
        case base of
            Table ts -> case op of
                S.PlusEq -> do
                    appendFuncName <- getTableAppendFunc (typeof val1)
                    val2 <- generateExpr (S.AExpr t2 expr2)
                    callWithParams [val1, val2] appendFuncName []
            _ -> error (show base)

    S.While _ expr stmt -> do
        id <- appendElem $ C.For Nothing Nothing Nothing []
        withCurID id $ do
            val <- generateExpr expr
            if_ (not_ val) $ appendElem C.Break
            generateStmt stmt
        

    S.For _ expr mpat stmt -> do
        base <- baseTypeOf expr
        idx <- assignI64 "idx" 0
        first <- assignBool "first" True

        id <- appendElem $ C.For Nothing Nothing (Just $ C.Increment (valExpr idx)) []
        withCurID id $ do
            val <- generateExpr expr
            -- special preable for ranges
            case base of
                Type.Range I64 -> do
                    if_ first $ do
                        set idx $ Value I64 (C.Member (valExpr val) "min")
                        set first false
                        return ()
                Type.String -> return ()
                Type.Array _ _ -> return ()
                Type.Table _ -> return ()

            -- check that index is still in range
            idxGtEq <- case base of
                Type.Range I64 -> generateInfix S.GTEq idx $ Value I64 (C.Member (valExpr val) "max")
                Type.String    -> generateInfix S.GTEq idx =<< len val
                Type.Array n t -> generateInfix S.GTEq idx (i64 n)
                Type.Table ts  -> generateInfix S.GTEq idx =<< len val
            if_ idxGtEq (appendElem C.Break)

                    
            -- check that pattern matches
            patMatches <- case mpat of
                Nothing -> return true
                Just pat -> case base of
                    Type.Range I64 -> generatePattern pat idx
                    Type.String    -> generatePattern pat =<< subscript val idx
                    Type.Array n t -> generatePattern pat =<< subscript val idx
                    Type.Table ts  -> generatePattern pat =<< subscript val idx

            if_ (not_ patMatches) $ appendElem C.Break
            generateStmt stmt

    _ -> error (show stmt)



-- TODO rewrite for expressions
-- creates an expression which may be used multiple times without side-effects
generateReentrantExpr :: MonadGenerate m => Value -> m Value
generateReentrantExpr (Value typ expr) = Value typ <$> reentrantExpr expr
    where
        reentrantExpr :: MonadGenerate m => C.Expression -> m C.Expression
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
            _ -> error (show expr)


generatePattern :: MonadGenerate m => Pattern -> Value -> m Value
generatePattern pattern val = do
    case pattern of
        PatIgnore _ -> return true
        PatLiteral expr -> generateInfix S.EqEq val =<< generateExpr expr

        PatArray _ pats -> do   
            base <- baseTypeOf val
            endLabel <- freshName "end"
            match <- assign "match" false

            -- check len
            case base of
                Type.Array n t -> do --[1, 2, 3]:[3 i64]
                    assert (n == length pats) "invalid number of patterns"
                Type.Table [t] -> do -- [1, 2, 3]:[i64]
                    lenNotEq <- generateInfix S.NotEq (i64 $ length pats) =<< len val
                    if_ lenNotEq $ void $ appendElem $ C.Goto endLabel

            forM_ (zip pats [0..]) $ \(pat, i) -> do
                b <- generatePattern pat =<< subscript val (i64 i)
                if_ (not_ b) $ appendElem $ C.Goto endLabel
                        
            set match true
            appendElem $ C.Label endLabel
            return match


        PatIdent _ symbol -> do 
            let name = show symbol
            define name (Value (typeof val) $ C.Ident name)
            cType <- cTypeOf (typeof val)
            appendAssign cType (show symbol) (valExpr val)
            return true

        PatTuple _ pats -> do
            base@(Type.Tuple ts) <- baseTypeOf val
            endLabel <- freshName "end"
            match <- assign "match" false

            forM_ (zip pats [0..]) $ \(pat, i) -> do
                b <- generatePattern pat =<< member i val
                if_ (not_ b) $ appendElem $ C.Goto endLabel
                        
            set match true
            appendElem $ C.Label endLabel
            return match


        PatGuarded _ pat expr Nothing -> do -- TODO
            match <- assign "match" =<< generatePattern pat val
            if_ match $ set match =<< generateExpr expr
            return match

        PatField _ symbol pats -> do -- either a typedef or an ADT field, both members of ADT
            base@(Type.ADT fs) <- baseTypeOf val
            isCtor <- Map.member symbol <$> gets ctors
            isTypedef <- Map.member symbol <$> gets typedefs

            endLabel <- freshName "skipMatch"

            i <- case (isCtor, isTypedef) of
                (True, False) -> do
                    (typ', i) <- (Map.! symbol) <$> gets ctors
                    assert (typ' == typeof val) "invalid ctor type"
                    let FieldCtor ts = fs !! i
                    assert (length pats == length ts) "invalid number of args"
                    return i

                (False, True) -> do
                    let typ = Type.Typedef symbol
                    i <- case elemIndex (FieldType typ) fs of
                        Just x -> return x
                        Nothing -> fail "type not in ADT"

                    assert (length pats == 1) "invalid number of args"
                    return i

            match <- assign "match" =<< generateInfix S.EqEq (i64 i) =<< adtEnum val
            if_ (not_ match) $ appendElem $ C.Goto endLabel
            set match false

            case (isCtor, isTypedef) of
                (True, False) -> do
                    let FieldCtor ts = fs !! i
                    assert (length pats == length ts) "invalid number of args"
                    case ts of
                        [] -> return ()
                        [t] -> do
                            patMatch <- generatePattern (head pats) =<< member i val
                            if_ (not_ patMatch) $ void $ appendElem (C.Goto endLabel)
                        ts -> do
                            forM_ (zip pats [0..]) $ \(pat, j) -> do
                                patMatch <- generatePattern pat =<< member j =<< member i val
                                if_ (not_ patMatch) $ void $ appendElem (C.Goto endLabel)

                (False, True) -> do
                    assert (length pats == 1) "invalid number of args"
                    patMatch <- generatePattern (head pats) =<< member i val
                    if_ (not_ patMatch) $ void $ appendElem (C.Goto endLabel)

            set match true
            appendElem $ C.Label endLabel
            return match

        PatNull _ -> do
            base@(Type.ADT fs) <- baseTypeOf val
            assert (Type.FieldNull `elem` fs) "ADT does not have a null field"
            let i = fromJust $ elemIndex Type.FieldNull fs
            assign "matchNull" =<< generateInfix S.EqEq (i64 i) =<< adtEnum val

        PatTypeField _ typ pat -> do
            base@(Type.ADT fs) <- baseTypeOf val
            let field = Type.FieldType typ
            assert (field `elem` fs) "ADT does not have a type field"

            let i = fromJust $ elemIndex field fs
            skip <- freshName "matchSkip"

            match <- assign "matchNull" =<< generateInfix S.EqEq (i64 i) =<< adtEnum val
            if_ (not_ match) $ appendElem (C.Goto skip)

            b <- generatePattern pat =<< member i val
            set match b
            appendElem $ C.Label skip
            return match

        _ -> error (show pattern)


-- generateExpr should return a 're-enter-able' expression, eg 1, not func()
generateExpr :: MonadGenerate m => Expr -> m Value
generateExpr (AExpr typ expr_) = withTypeCheck $ case expr_ of
    S.Bool _ b -> return $ Value typ (C.Bool b)
    S.Int _ n -> return $ Value typ (C.Int n)
    S.Float _ f -> return $ Value typ (C.Float f)
    S.Ident _ symbol -> look (show symbol)
    S.String _ s -> return $ Value typ $ C.String s
    S.Char _ c -> return $ Value typ $ C.Char c
    S.Match _ expr pattern -> generatePattern pattern =<< generateExpr expr
    S.Builtin _ [] "len" [expr] -> len =<< generateExpr expr

    S.Null _ -> do
        base <- baseTypeOf typ
        case base of
            Type.ADT fs -> do
                assert (Type.FieldNull `elem` fs) "ADT type does not have a null"
                let i = fromJust $ elemIndex Type.FieldNull fs
                assign "adt" $ Value typ $ C.Initialiser [C.Int $ fromIntegral i]
            _ -> error (show base)

    S.Prefix _ op a -> do
        val <- generateExpr a
        base <- baseTypeOf val
        case base of
            Type.Bool -> case op of
                S.Not -> return (not_ val)
            Type.I64 -> case op of
                S.Minus -> generateInfix S.Minus (i64 0) val

    S.Infix _ op a b -> do
        valA <- generateExpr a
        valB <- generateExpr b
        generateInfix op valA valB

    S.Call _ exprs1 symbol exprs2 -> do
        objs1 <- mapM generateExpr exprs1
        objs2 <- mapM generateExpr exprs2
        return $ Value typ $ C.Call (show symbol) (map ptrExpr objs1 ++ map valExpr objs2)

    S.Field _ expr symbol -> do 
        val <- generateExpr expr
        base <- baseTypeOf val
        (typ, i) <- (Map.! symbol) <$> gets ctors
        assert (typ == typeof val) "ctor type mismatch"
        member i val

    S.Tuple _ exprs -> do
        vals <- mapM generateExpr exprs
        base <- baseTypeOf typ
        case base of
            Type.Table ts -> do
                assert (length ts == length exprs) "invalid table type"
                ptrs <- forM (zip ts exprs) $ \(t, e) -> do
                    v <- generateExpr e
                    b <- baseTypeOf v
                    assert (b == Table [t]) "invalid row type"
                    return $ C.Member (valExpr v) "r0"
                table <- assign "table" $ Value typ (C.Initialiser
                    ([ C.Member (valExpr $ head vals) "len"
                    , C.Member (valExpr $ head vals) "cap"] ++ ptrs))

                return table

            _ -> assign "tuple" $ Value typ (C.Initialiser $ map valExpr vals)


    S.Conv _ t [expr] -> do
        val <- generateExpr expr
        base <- baseTypeOf t
        baseVal <- baseTypeOf val
        case base of
            Type.ADT fs -> generateAdtInit t val
            Type.I64 -> do
                case baseVal of
                    Type.Char -> return $ Value t (valExpr val)
                    _ -> error (show baseVal)

            Type.String -> do
                case baseVal of
                    Type.Char -> return $ Value t $ C.Call "doodad_string_char" [valExpr val]
                    Type.String -> return $ Value t (valExpr val)
                    _ -> error (show baseVal)

            Type.F32 -> do
                case baseVal of
                    Type.F64 -> return $ Value t $ C.Cast Cfloat (valExpr val)
                    Type.I64 -> return $ Value t $ C.Cast Cfloat (valExpr val)

            _ -> error (show base)

    S.Array _ exprs -> do
        base <- baseTypeOf typ
        case base of
            Type.Array n t -> do
                assert (n == length exprs) "incorrect array length"
                vals <- mapM generateExpr exprs
                assign "array" $ Value typ (C.Initialiser $ map valExpr vals)

            Type.Table [t] -> do
                vals <- mapM generateExpr exprs
                let len = length vals
                array <- assign "tabMem" $ Value (Type.Array len t) $
                    (C.Initialiser $ map valExpr vals)
                assign "table" $ Value typ $
                    C.Initialiser [C.Int (fromIntegral len), C.Int (fromIntegral len), C.Member (valExpr array) "arr"]

    S.Range _ (Just expr) mexpr1 mexpr2 -> do
        val <- generateExpr expr
        base <- baseTypeOf val

        start <- case base of
            Type.Array n t -> case mexpr1 of
                Nothing -> return (i64 0)
            Type.Table ts -> case mexpr1 of
                Nothing -> return (i64 0)
        end <- case base of
            Type.Array n t -> case mexpr2 of
                Nothing -> return (i64 n)
            Type.Table ts -> case mexpr2 of
                Nothing -> len val

        assign "range" $ Value typ (C.Initialiser [valExpr start, valExpr end])


    S.Range _ Nothing (Just expr1) (Just expr2) -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        assert (typeof val1 == typeof val2) "type mismatch"
        assign "range" $ Value typ (C.Initialiser [valExpr val1, valExpr val2])

    S.Construct _ symbol exprs -> do
        base <- baseTypeOf typ
        vals <- mapM generateExpr exprs
        (typ', i) <- (Map.! symbol) <$> gets ctors
        assert (typ == typ') "error, types don't match"

        case base of
            Type.ADT fs -> do
                assert (i < length fs) "invalid index"
                case fs !! i of
                    FieldCtor ts -> do
                        assert (length vals == length ts) "invalid constructor"

                        adt <- assign "adt" $ Value typ (C.Initialiser [C.Int $ fromIntegral i]) -- TODO
                        forM_ (zip vals [0..]) $ \(v, j) -> do
                            m <- member j =<< member i adt
                            set m v
                        return adt

                    _ -> error (show $ fs !! i)

            _ -> error (show typ)

    S.Conv pos typ exprs -> do -- construct 0
        vals <- mapM generateExpr exprs
        case vals of
            [] -> assign "zero" $ Value typ $ (C.Initialiser [C.Int 0])
            _  -> assign "ctor" $ Value typ $ (C.Initialiser $ map valExpr vals)

    S.Subscript _ expr1 expr2 -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        subscript val1 =<< generateExpr expr2

    _ -> error (show expr_)
    where
        withTypeCheck :: MonadGenerate m => m Value -> m Value
        withTypeCheck f = do
            r <- generateReentrantExpr =<< f
            assert (typeof r == typ) $ 
                "generateExpr returned: " ++ show r ++ " but checked " ++ show typ ++ " for " ++ show expr_
            return r
            



generateInfix :: MonadGenerate m => S.Operator -> Value -> Value -> m Value
generateInfix op a b = do
    assert (typeof a == typeof b) $ "infix type mismatch: " ++ show (typeof a) ++ ", " ++ show (typeof b)
    base <- baseTypeOf a
    case base of
        Type.F64 -> return $ case op of
            S.Plus ->   Value (typeof a) $ C.Infix C.Plus (valExpr a) (valExpr b) 
            S.Times ->  Value (typeof a) $ C.Infix C.Times (valExpr a) (valExpr b) 
            S.Minus ->  Value (typeof a) $ C.Infix C.Minus (valExpr a) (valExpr b)
            S.Modulo -> Value (typeof a) $ C.Infix C.Modulo (valExpr a) (valExpr b)
            S.LT ->     Value Type.Bool $ C.Infix C.LT (valExpr a) (valExpr b)
            S.GT ->     Value Type.Bool $ C.Infix C.GT (valExpr a) (valExpr b)
            S.LTEq ->   Value Type.Bool $ C.Infix C.LTEq (valExpr a) (valExpr b)
            S.EqEq ->   Value Type.Bool $ C.Infix C.EqEq (valExpr a) (valExpr b)
            S.GTEq ->   Value Type.Bool $ C.Infix C.EqEq (valExpr a) (valExpr b)
            S.NotEq ->  Value Type.Bool $ C.Infix C.NotEq (valExpr a) (valExpr b)
            _ -> error (show op)

        Type.I64 -> return $ case op of
            S.Plus ->   Value (typeof a) $ C.Infix C.Plus (valExpr a) (valExpr b) 
            S.Times ->  Value (typeof a) $ C.Infix C.Times (valExpr a) (valExpr b) 
            S.Minus ->  Value (typeof a) $ C.Infix C.Minus (valExpr a) (valExpr b)
            S.Modulo -> Value (typeof a) $ C.Infix C.Modulo (valExpr a) (valExpr b)
            S.LT ->     Value Type.Bool $ C.Infix C.LT (valExpr a) (valExpr b)
            S.LTEq ->   Value Type.Bool $ C.Infix C.LTEq (valExpr a) (valExpr b)
            S.EqEq ->   Value Type.Bool $ C.Infix C.EqEq (valExpr a) (valExpr b)
            S.GTEq ->   Value Type.Bool $ C.Infix C.EqEq (valExpr a) (valExpr b)
            S.NotEq ->  Value Type.Bool $ C.Infix C.NotEq (valExpr a) (valExpr b)
            _ -> error (show op)

        Type.Bool -> return $ case op of
            S.AndAnd -> Value (typeof a) $ C.Infix C.AndAnd (valExpr a) (valExpr b)
            S.OrOr   -> Value (typeof a) $ C.Infix C.OrOr (valExpr a) (valExpr b)
            S.EqEq   -> Value (typeof a) $ C.Infix C.EqEq (valExpr a) (valExpr b)
            _ -> error (show op)

        Type.Char -> return $ case op of
            S.EqEq -> Value Type.Bool $ C.Infix (C.EqEq) (valExpr a) (valExpr b)
            o -> error (show o)

        Type.String -> case op of
            S.Plus -> return $ Value (typeof a) (C.Call "doodad_string_plus" [valExpr a, valExpr b])
            S.EqEq -> return $ Value Type.Bool  (C.Call "doodad_string_eqeq" [valExpr a, valExpr b])
            _ -> error (show op)

        Type.Tuple ts -> case op of
            S.EqEq -> do
                end <- freshName "end" 
                eq <- assign "eq" false
                forM_ (zip ts [0..]) $ \(t, i) -> do
                    ma <- member i a
                    mb <- member i b
                    b <- generateInfix S.EqEq ma mb
                    if_ (not_ b) $ appendElem $ C.Goto end
                set eq true
                appendElem $ C.Label end
                return eq
            S.NotEq -> do
                end <- freshName "end" 
                eq <- assign "eq" false
                forM_ (zip ts [0..]) $ \(t, i) -> do
                    ma <- member i a
                    mb <- member i b
                    b <- generateInfix S.EqEq ma mb
                    if_ (not_ b) $ appendElem $ C.Goto end
                set eq true
                appendElem $ C.Label end
                return (not_ eq)

            S.Plus -> do
                vals <- forM (zip ts [0..]) $ \(t, i) -> do
                    ma <- member i a
                    mb <- member i b
                    generateInfix op ma mb
                assign "infix" $ Value (Type.Tuple $ map typeof vals) $ C.Initialiser (map valExpr vals)

        Type.Array n t -> case op of
            S.EqEq -> do
                idx <- assignI64 "idx" 0
                eq <- assignBool "eq" True

                forId <- appendElem $ C.For
                    Nothing
                    (Just (C.Infix C.LT (valExpr idx) (C.Int $ fromIntegral n)))
                    (Just (C.Increment $ valExpr idx))
                    []
                withCurID forId $ do
                    elemA <- subscript a idx
                    elemB <- subscript b idx
                    b <- generateInfix S.EqEq elemA elemB
                    if_ (not_ b) $ do
                        set eq false
                        appendElem $ C.Break
                return eq

        Type.ADT fs -> case op of
            S.NotEq -> not_ <$> generateAdtEqual a b
            S.EqEq -> generateAdtEqual a b
                    

        _ -> error $ show (base, op)