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
            generateFunc (Sym "main") $ FuncBody
                { States.funcArgs = []
                , States.funcParams = []
                , States.funcRetty = Type.Void
                , States.funcStmts = [S.ExprStmt $ S.AExpr Type.Void $ S.Call undefined [] symbol []]
                }



generateFunc :: MonadGenerate m => Symbol -> FuncBody -> m ()
generateFunc symbol body = do
    args <- mapM cParamOf (States.funcArgs body)
    params <- map (\(C.Param n t) -> C.Param n (Cpointer t)) <$> mapM cParamOf (States.funcParams body)
    rettyType <- cTypeOf (States.funcRetty body)

    forM_ (States.funcParams body) $ \param -> do
        ctyp <- cTypeOf (paramType param)
        name <- return $ show (S.paramName param)
        define name $ Pointer (S.paramType param) (C.Ident name)

    forM_ (States.funcArgs body) $ \arg -> do
        ctyp <- cTypeOf (paramType arg)
        name <- return $ show (S.paramName arg)
        define name $ Value (S.paramType arg) (C.Ident name)

    id <- newFunction rettyType (show symbol) (params ++ args)
    withCurID id $ do
        mapM_ generateStmt (States.funcStmts body)
        when (States.funcRetty body /= Type.Void) $ -- check to ensure function has return
            void $ appendElem $ C.ExprStmt $ C.Call "assert" [C.Bool False]
    withCurID globalID $ append id



generatePrint :: MonadGenerate m => String -> Object -> m ()
generatePrint app val = case typeof val of
    Type.I64 ->    void $ appendPrintf ("%d" ++ app) [valueExpr val]
    Type.F64 ->    void $ appendPrintf ("%f" ++ app) [valueExpr val]
    Type.String -> void $ appendPrintf ("%s" ++ app) [valueExpr val]
    Type.Char ->   void $ appendPrintf ("%c" ++ app) [valueExpr val]

    Type.Bool -> void $ appendPrintf ("%s" ++ app) $
        [C.CndExpr (valueExpr val) (C.String "true") (C.String "false")]

    Type.Tuple ts -> do
        appendElem $ C.ExprStmt $ C.Call "putchar" [C.Char '(']
        forM_ (zip ts [0..]) $ \(t, i) -> do
            let end = i == length ts - 1
            generatePrint (if end then "" else ", ") =<< generateTupleIndex val i
        void $ appendPrintf (")" ++ app) []

    _ -> error (show $ typeof val)


generateStmt :: MonadGenerate m => S.Stmt -> m ()
generateStmt stmt = case stmt of
    S.EmbedC _ str -> do void $ appendElem (C.Embed str)
    S.Block stmts -> mapM_ generateStmt stmts
    S.Return _ Nothing -> do void $ appendElem (C.ReturnVoid)
    S.Return _ (Just expr) -> do void $ appendElem . C.Return . valueExpr =<< generateExpr expr

    S.Assign _ pattern expr -> do
        val <- generateExpr expr
        matched <- generatePattern pattern val
        appendElem $ C.ExprStmt $ C.Call "assert" [matched]
        return ()

    S.Data _ symbol typ Nothing -> do
        ctyp <- cTypeOf typ
        appendAssign ctyp (show symbol) (C.Initialiser [C.Int 0])
        define (show symbol) $ Value typ $ C.Ident (show symbol)
        

    S.If _ expr blk melse -> do
        val <- generateExpr expr
        ifID <- appendIf (valueExpr val)
        withCurID ifID $ generateStmt blk
        when (isJust melse) $ do
            elseID <- appendElem $ C.Else { elseStmts = [] }
            withCurID elseID $ generateStmt (fromJust melse)

    S.Switch _ cnd cases -> do
        val <- generateExpr cnd
        name <- freshName "switchExpr"
        ctyp <- cTypeOf val
        appendAssign ctyp name (valueExpr val)
        let newVal = Value (typeof val) (C.Ident name)

        switchId <- newElement $ C.Switch { switchBody = [], switchExpr = C.Int 0 }
        caseId <- newElement $ C.Case { caseExpr = C.Int 0, caseBody = [] }
        withCurID switchId $ append caseId

        forM_ cases $ \(pattern, stmt) -> do
            ifId <- withCurID caseId $ appendIf =<< withCurID caseId (generatePattern pattern newVal)
            withCurID ifId $ do
                generateStmt stmt
                appendElem C.Break

        withCurID caseId $ appendElem (C.ExprStmt (C.Call "assert" [C.Int 0]))
        append switchId

    S.ExprStmt (AExpr _ (S.Call _ exprs1 symbol exprs2)) -> do
        objs1 <- mapM generateExpr exprs1
        objs2 <- mapM generateExpr exprs2
        void $ appendElem $ C.ExprStmt $
            C.Call (show symbol) (map pointerExpr objs1 ++ map valueExpr objs2)

    S.ExprStmt (AExpr _ (S.Builtin _ [] "print" exprs)) -> do
        vals <- mapM generateExpr exprs
        forM_ (zip vals [0..]) $ \(val, i) -> do
            let end = i == length vals - 1
            generatePrint (if end then "\n" else ", ") val

    S.Set _ expr1 expr2 -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        void $ appendElem $ C.Set (valueExpr val1) (valueExpr val2)

    S.SetOp _ op expr1 (S.AExpr t2 expr2) -> do
        val1 <- generateExpr expr1
        assert (typeof val1 == t2) "types do not match"
        base <- baseTypeOf val1
        case base of
            Table [t] -> case op of
                S.PlusEq -> case expr2 of
                    S.Array _ es -> do
                        appendFuncName <- getTableAppendFunc (typeof val1)
                        forM_ es $ \e -> do
                            val2 <- generateExpr e
                            assert (typeof val2 == t) "types do not match"
                            appendElem $ C.ExprStmt $
                                C.Call appendFuncName [C.Address (valueExpr val1), C.Address (valueExpr val2)]

            Table ts -> case op of
                S.PlusEq -> case expr2 of
                    S.Tuple _ es -> do
                        error "don't know"
                    
                
            _ -> error (show base)

    S.While _ expr stmt -> do
        id <- appendElem $ C.For Nothing Nothing Nothing []
        withCurID id $ do
            val <- generateExpr expr
            ifId <- appendIf (C.Not $ valueExpr val)
            withCurID ifId $ appendElem C.Break
            generateStmt stmt
        

    S.For _ expr mpat stmt -> do
        base <- baseTypeOf expr
        idxType <- case base of
            Type.Range I64 -> cTypeOf I64
            Type.String    -> cTypeOf I64
            Type.Array _ _ -> cTypeOf I64

        idxName <- freshName "index"
        appendAssign idxType idxName (C.Int 0)

        firstName <- freshName "isFirst"
        appendAssign Cbool firstName (C.Bool True)


        id <- appendElem $ C.For Nothing Nothing (Just $ C.Increment (C.Ident idxName)) []
        withCurID id $ do
            val <- generateExpr expr
            -- special preable for ranges
            case base of
                Type.Range I64 -> do
                    ifID <- appendIf (C.Ident firstName)
                    void $ withCurID ifID $ do
                        appendElem $ C.Set (C.Ident idxName) (C.Member (valueExpr val) "min")
                        appendElem $ C.Set (C.Ident firstName) (C.Bool False)

                Type.String -> return ()
                Type.Array _ _ -> return ()

            -- check that index is still in range
            case base of
                Type.Range I64 -> do
                    ifId <- appendIf $
                        C.Infix C.GTEq (C.Ident idxName) (C.Member (valueExpr val) "max")
                    withCurID ifId $ appendElem C.Break
                Type.String -> do
                    ifId <- appendIf $
                        C.Infix C.GTEq (C.Ident idxName) (C.Call "strlen" [valueExpr val])
                    withCurID ifId $ appendElem C.Break
                Type.Array n t -> do
                    ifId <- appendIf $
                        C.Infix C.GTEq (C.Ident idxName) (C.Int $ fromIntegral n)
                    withCurID ifId $ appendElem C.Break

                    
            -- check that pattern matches
            patMatches <- case mpat of
                Nothing -> return (C.Bool True)
                Just pat -> case base of
                    Type.Range I64 -> generatePattern pat (Value I64 $ C.Ident idxName)
                    Type.String    -> generatePattern pat (Value Type.Char $ C.Subscript (valueExpr val) (C.Ident idxName))
                    Type.Array n t -> generatePattern pat (Value t $ C.Subscript (C.Member (valueExpr val) "arr") (C.Ident idxName))

            ifId <- appendIf (C.Not patMatches)
            withCurID ifId $ appendElem C.Break
            generateStmt stmt

    _ -> error (show stmt)



generateTupleIndex :: MonadGenerate m => Object -> Int -> m Object
generateTupleIndex obj i = do
    base@(Type.Tuple ts) <- baseTypeOf obj
    case obj of
        Value _ e   -> return $ Value (ts !! i) (C.Member e $ "m" ++ show i)
        Pointer _ e -> return $ Value (ts !! i) (C.PMember e $ "m" ++ show i)


-- creates an expression which may be used multiple times without side-effects
generateReentrantExpr :: MonadGenerate m => Object -> m Object
generateReentrantExpr obj = case obj of
    Value _ (C.Ident _) -> return obj
    Value _ (C.Char _) -> return obj
    Value _ (C.Int _) -> return obj
    Value _ (C.Float _) -> return obj
    Value _ (C.Bool _) -> return obj
    Value _ (C.String s) | length s <= 16 -> return obj
    Value _ (C.Subscript e1 e2) -> return obj
    Value _ (C.Member e1 e2) -> return obj
    Pointer _ (C.Ident _) -> return obj
    Value t expr -> do
        name <- freshName $ case expr of
            C.Infix _ _ _ -> "infix"
            C.Call _ _ -> "call"
            _ -> "var"
        ctyp <- cTypeOf t
        appendAssign ctyp name expr
        return $ Value t $ C.Ident name


generateAdtEnum :: MonadGenerate m => Object -> m Object
generateAdtEnum obj = do
    base@(Type.ADT fs) <- baseTypeOf obj
    return $ Value I64 $ C.Member (valueExpr obj) "en"

generateAdtInit :: MonadGenerate m => Type.Type -> Object -> m Object
generateAdtInit typ val = do
    base@(Type.ADT fs) <- baseTypeOf typ
    let field = Type.FieldType (typeof val)
    assert (field `elem` fs) "TODO"
    let i = fromJust $ elemIndex field fs
    name <- freshName "adt"
    ctyp <- cTypeOf typ
    appendAssign ctyp name $ C.Initialiser [C.Int $ fromIntegral i]
    appendElem $ C.Set (C.Member (C.Ident name) ("u" ++ show i)) (valueExpr val)
    return $ Value typ $ C.Ident name

generateAdtEqual :: MonadGenerate m => Object -> Object -> m Object
generateAdtEqual a b = do
    assert (typeof a == typeof b) "types aren't equal"
    base@(Type.ADT fs) <- baseTypeOf a

    enA <- generateAdtEnum a
    enB <- generateAdtEnum b
    eqName <- freshName "adtEqual"
    appendAssign Cbool eqName (C.Infix C.EqEq (valueExpr enA) (valueExpr enB))
    
    ifId <- appendIf (C.Ident eqName)
    withCurID ifId $ do
        switchId <- appendElem $ C.Switch (valueExpr enA) []
        withCurID switchId $ do
            forM_ (zip fs [0..]) $ \(field, i) -> case field of
                FieldNull -> return ()
                FieldCtor [] -> return ()
                -- set eq to false if we have a field to check

    return $ Value Type.Bool $ C.Ident eqName


generatePattern :: MonadGenerate m => Pattern -> Object -> m C.Expression
generatePattern pattern val = do
    case pattern of
        PatIgnore _ -> return (C.Bool True)

        PatArray _ pats -> do   
            base <- baseTypeOf val
            case base of
                Type.Array n t -> do -- TODO cheating
                    assert (n == length pats) "invalid number of patterns"
                    bs <- forM (zip pats [0..]) $ \(pat, i) -> do
                        generatePattern pat $
                            Value t (C.Subscript (C.Member (valueExpr val) "arr") (C.Int $ fromIntegral i))
                    name <- freshName "match"
                    appendAssign Cbool name (foldr1 (C.Infix C.AndAnd) bs)
                    return $ C.Ident name

        PatIdent _ symbol -> do 
            let name = show symbol
            define name (Value (typeof val) $ C.Ident name)
            cType <- cTypeOf (typeof val)
            appendAssign cType (show symbol) (valueExpr val)
            return (C.Bool True)

        PatLiteral expr -> do
            v <- generateExpr expr
            valueExpr <$> generateInfix S.EqEq v val

        PatTuple _ pats -> do
            base@(Type.Tuple ts) <- baseTypeOf val
            assert (length ts == length pats) "length mismatch"
            bs <- forM (zip pats [0..]) $ \(pat, i) -> do
                generatePattern pat =<< generateTupleIndex val i

            -- TODO cheating
            name <- freshName "match"
            appendAssign Cbool name (foldr1 (C.Infix C.AndAnd) bs)
            return $ C.Ident name

        PatArray _ [pats] -> do
            error (show pats)

        PatGuarded _ pat expr Nothing -> do -- TODO
            match <- freshName "match"
            appendAssign Cbool match =<< generatePattern pat val
            ifId <- appendIf (C.Ident match)
            withCurID ifId $ do
                v <- generateExpr expr
                appendElem (C.Set (C.Ident match) (valueExpr v))
            return (C.Ident match)

        PatField _ symbol pats -> do -- either a typedef or an ADT field, both members of ADT
            base@(Type.ADT fs) <- baseTypeOf val
            isCtor <- Map.member symbol <$> gets ctors
            isTypedef <- Map.member symbol <$> gets typedefs

            endLabel <- freshName "skipMatch"
            match <- freshName "match"

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

            appendAssign Cbool match . C.Infix C.EqEq (C.Int $ fromIntegral i) . valueExpr
                =<< generateAdtEnum val
            ifId <- appendIf (C.Not $ C.Ident match)
            withCurID ifId $ do
                appendElem $ C.Goto endLabel

            appendElem $ C.Set (C.Ident match) (C.Bool False)

            case (isCtor, isTypedef) of
                (True, False) -> do
                    let FieldCtor ts = fs !! i
                    assert (length pats == length ts) "invalid number of args"

                    forM_ (zip3 pats ts [0..]) $ \(pat, t, j) -> do
                        patMatch <- generatePattern pat $ Value t $
                            C.Member (C.Member (valueExpr val) ("u" ++ show i)) ("m" ++ show j)
                        ifId <- appendIf (C.Not patMatch)
                        withCurID ifId $ do
                            appendElem $ C.Goto endLabel

                (False, True) -> do
                    assert (length pats == 1) "invalid number of args"
                    let typ = Type.Typedef symbol
                    let pat = head pats

                    patMatch <- generatePattern pat $ Value typ $
                        C.Member (valueExpr val) ("u" ++ show i)
                    ifId <- appendIf (C.Not patMatch)
                    withCurID ifId $ do
                        void $ appendElem $ C.Goto endLabel

            appendElem $ C.Set (C.Ident match) (C.Bool True)
            appendElem $ C.Label endLabel
            return (C.Ident match)

        PatNull _ -> do
            base@(Type.ADT fs) <- baseTypeOf val
            assert (Type.FieldNull `elem` fs) "ADT does not have a null field"
            let i = fromJust $ elemIndex Type.FieldNull fs
            name <- freshName "matchNull"
            appendAssign Cbool name $
                C.Infix C.EqEq (C.Int $ fromIntegral i) (C.Member (valueExpr val) "en")
            return $ C.Ident name


        PatTypeField _ typ pat -> do
            base@(Type.ADT fs) <- baseTypeOf val
            let field = Type.FieldType typ
            assert (field `elem` fs) "ADT does not have a type field"

            let i = fromJust $ elemIndex field fs
            skip <- freshName "matchSkip"
            name <- freshName "matchNull"
            appendAssign Cbool name $
                C.Infix C.EqEq (C.Int $ fromIntegral i) (C.Member (valueExpr val) "en")
            ifId <- appendIf (C.Not $ C.Ident name)
            withCurID ifId $ do
                appendElem $ C.Goto skip

            b <- generatePattern pat $ Value typ $ C.Member (valueExpr val) ("u" ++ show i)
            appendElem $ C.Set (C.Ident name) b

            appendElem $ C.Label skip
            return $ C.Ident name

        _ -> error (show pattern)


-- generateExpr should return a 're-enter-able' expression, eg 1, not func()
generateExpr :: MonadGenerate m => Expr -> m Object
generateExpr (AExpr typ expr_) = withTypeCheck $ case expr_ of
    S.Bool _ b -> return $ Value typ (C.Bool b)
    S.Int _ n -> return $ Value typ (C.Int n)
    S.Float _ f -> return $ Value typ (C.Float f)
    S.Ident _ symbol -> look (show symbol)
    S.String _ s -> return $ Value typ $ C.String s
    S.Char _ c -> return $ Value typ $ C.Char c

    S.Null _ -> do
        base <- baseTypeOf typ
        case base of
            Type.ADT fs -> do
                assert (Type.FieldNull `elem` fs) "ADT type does not have a null"
                let i = fromJust $ elemIndex Type.FieldNull fs
                ctyp <- cTypeOf typ
                name <- freshName "adt"
                appendAssign ctyp name $ C.Initialiser [C.Int $ fromIntegral i]
                return $ Value typ $ C.Ident name

    S.Prefix _ op a -> do
        val <- generateExpr a
        base <- baseTypeOf val
        case base of
            Type.Bool -> case op of
                S.Not -> return $ Value typ $ C.Not (valueExpr val)

    S.Infix _ op a b -> do
        valA <- generateExpr a
        valB <- generateExpr b
        generateInfix op valA valB

    S.Call _ exprs1 symbol exprs2 -> do
        objs1 <- mapM generateExpr exprs1
        objs2 <- mapM generateExpr exprs2
        return $ Value typ $ C.Call (show symbol) (map pointerExpr objs1 ++ map valueExpr objs2)

    S.Tuple _ exprs -> do
        vals <- mapM generateExpr exprs
        cType <- cTypeOf typ
        name <- freshName "tuple"
        id <- appendAssign cType name (C.Initialiser $ map valueExpr vals)
        return $ Value typ $ C.Ident name

    S.Builtin _ [] "len" [expr] -> do
        val <- generateExpr expr
        base <- baseTypeOf val
        case base of
            Type.Array n t -> return $ Value typ $ C.Int (fromIntegral n)
            Type.String    -> return $ Value typ $ C.Call "strlen" [valueExpr val]
            Type.Table ts  -> return $ Value typ $ C.Member (valueExpr val) "len"

    S.Conv _ t [expr] -> do
        val <- generateExpr expr
        base <- baseTypeOf t
        case base of
            Type.ADT fs -> generateAdtInit t val

            Type.I64 -> do
                baseVal <- baseTypeOf val
                case baseVal of
                    Type.Char -> return $ Value t (valueExpr val)
                    _ -> error (show baseVal)

            Type.String -> do
                baseVal <- baseTypeOf val
                case baseVal of
                    Type.Char -> return $ Value t $ C.Call "doodad_string_char" [valueExpr val]

            _ -> error (show base)

    S.Array _ exprs -> do
        base <- baseTypeOf typ
        case base of
            Type.Array n t -> do
                assert (n == length exprs) "incorrect array length"
                vals <- mapM generateExpr exprs
                name <- freshName "array"
                ctyp <- cTypeOf typ
                appendAssign ctyp name (C.Initialiser $ map valueExpr vals)
                return $ Value typ $ C.Ident name

    S.Match _ expr pattern -> do
        val <- generateExpr expr
        Value typ <$> generatePattern pattern val

    S.Range _ (Just expr) mexpr1 mexpr2 -> do
        val <- generateExpr expr
        base <- baseTypeOf val
        case base of
            Type.Array n t -> do
                start <- case mexpr1 of
                    Nothing -> return $ Value Type.I64 $ C.Int 0

                end <- case mexpr2 of
                    Nothing -> return $ Value Type.I64 $ C.Int (fromIntegral n)

                ctype <- cTypeOf typ
                name <- freshName "range"
                id <- appendElem $
                    C.Assign ctype name (C.Initialiser [valueExpr start, valueExpr end])
                return $ Value typ $ C.Ident name

    S.Range _ Nothing (Just expr1) (Just expr2) -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        assert (typeof val1 == typeof val2) "type mismatch"
        ctype <- cTypeOf typ
        name <- freshName "range"
        appendAssign ctype name (C.Initialiser [valueExpr val1, valueExpr val2])
        return $ Value typ $ C.Ident name

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

                        name <- freshName "adt"
                        ctyp <- cTypeOf typ
                        appendAssign ctyp name (C.Initialiser [C.Int $ fromIntegral i]) -- TODO
                        forM_ (zip vals [0..]) $ \(val, j) -> do
                            appendElem $ C.Set
                                (C.Member (C.Member (C.Ident name) ("u" ++ show i)) ("m" ++ show j))
                                (valueExpr val)

                        return $ Value typ $ C.Ident name

                    _ -> error (show $ fs !! i)

            _ -> error (show typ)

    S.Conv pos typ exprs -> do -- construct 0
        vals <- mapM generateExpr exprs
        name <- freshName "zero"
        ctyp <- cTypeOf typ
        case vals of
            [] -> appendAssign ctyp name (C.Initialiser [C.Int 0])
            _  -> appendAssign ctyp name (C.Initialiser $ map valueExpr vals)
        return $ Value typ (C.Ident name)

    S.Subscript _ expr1 expr2 -> do
        val1 <- generateExpr expr1
        val2 <- generateExpr expr2
        base <- baseTypeOf val1
        case base of
            Type.Array n t -> do
                return $ Value typ $ C.Subscript (C.Member (valueExpr val1) "arr") (valueExpr val2)
            Type.String -> do
                return $ Value typ $ C.Subscript (valueExpr val1) (valueExpr val2)
            Type.Table [t] -> do
                return $ Value typ $ C.Subscript (C.Member (valueExpr val1) "r0") (valueExpr val2)
            _ -> error (show base)

    _ -> error (show expr_)
    where
        withTypeCheck :: MonadGenerate m => m Object -> m Object
        withTypeCheck f = do
            r <- generateReentrantExpr =<< f
            assert (typeof r == typ) $ 
                "generateExpr returned: " ++ show r ++ " but checked " ++ show typ
            return r
            



generateInfix :: MonadGenerate m => S.Operator -> Object -> Object -> m Object
generateInfix op a b = do
    assert (typeof a == typeof b) "infix types do not match"
    base <- baseTypeOf a
    case base of
        Type.I64 -> return $ case op of
            S.Plus ->   Value (typeof a) $ C.Infix C.Plus (valueExpr a) (valueExpr b) 
            S.Times ->  Value (typeof a) $ C.Infix C.Times (valueExpr a) (valueExpr b) 
            S.Minus ->  Value (typeof a) $ C.Infix C.Minus (valueExpr a) (valueExpr b)
            S.Modulo -> Value (typeof a) $ C.Infix C.Modulo (valueExpr a) (valueExpr b)
            S.LT ->     Value Type.Bool $ C.Infix C.LT (valueExpr a) (valueExpr b)
            S.LTEq ->   Value Type.Bool $ C.Infix C.LTEq (valueExpr a) (valueExpr b)
            S.EqEq ->   Value Type.Bool $ C.Infix C.EqEq (valueExpr a) (valueExpr b)
            S.GTEq ->   Value Type.Bool $ C.Infix C.EqEq (valueExpr a) (valueExpr b)
            _ -> error (show op)

        Type.Bool -> return $ case op of
            S.AndAnd -> Value (typeof a) $ C.Infix C.AndAnd (valueExpr a) (valueExpr b)
            S.OrOr   -> Value (typeof a) $ C.Infix C.OrOr (valueExpr a) (valueExpr b)
            S.EqEq   -> Value (typeof a) $ C.Infix C.EqEq (valueExpr a) (valueExpr b)
            _ -> error (show op)

        Type.Char -> return $ case op of
            S.EqEq -> Value Type.Bool $ C.Infix (C.EqEq) (valueExpr a) (valueExpr b)
            o -> error (show o)

        Type.String -> case op of
            S.Plus -> return $ Value (typeof a) (C.Call "doodad_string_plus" [valueExpr a, valueExpr b])
            S.EqEq -> return $ Value Type.Bool  (C.Call "doodad_string_eqeq" [valueExpr a, valueExpr b])
            _ -> error (show op)

        Type.Array n t -> case op of
            S.EqEq -> do
                idxName <- freshName "index"
                eqName <- freshName "equal"
                appendAssign Cint64_t idxName (C.Int 0)
                appendAssign Cbool eqName (C.Bool True)
                forId <- appendElem $ C.For
                    Nothing
                    (Just (C.Infix C.LT (C.Ident idxName) (C.Int $ fromIntegral n)))
                    (Just (C.Increment $ C.Ident idxName))
                    []
                withCurID forId $ do
                    let elemA = Value t $ C.Subscript (C.Member (valueExpr a) "arr") (C.Ident idxName)
                    let elemB = Value t $ C.Subscript (C.Member (valueExpr b) "arr") (C.Ident idxName)
                    b <- generateInfix S.EqEq elemA elemB
                    ifId <- appendIf (C.Not $ valueExpr b)
                    withCurID ifId $ do
                        appendElem $ C.Set (C.Ident eqName) (C.Bool False)
                        appendElem $ C.Break
                return $ Value Type.Bool $ C.Ident eqName

        Type.ADT fs -> case op of
            S.NotEq -> do
                eq <- generateAdtEqual a b
                return $ Value Type.Bool $ (C.Not $ valueExpr eq)

            S.EqEq -> generateAdtEqual a b
                    

        _ -> error $ show base
