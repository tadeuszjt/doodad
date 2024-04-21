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
    modify $ \s -> s { typefuncs = typeFuncs ast } 

    -- generate imported function externs
    forM_ (Map.toList $ funcImports ast) $ \(symbol, body) -> do
        unless (isGenericBody body) $ do
            case (ASTResolved.funcRetty body) of
                Retty _    -> addFuncRefType symbol False
                RefRetty _ -> addFuncRefType symbol True
                _ -> return ()

            crt <- cRettyType (ASTResolved.funcRetty body)
            cats <- forM (ASTResolved.funcArgs body) $ \param -> case param of
                S.Param _ _ _ -> cTypeOf param
                S.RefParam _ _ _ -> cRefTypeOf param
                x -> error (show x)
            newExtern (show symbol) crt cats

    -- generate function headers
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
         unless (isGenericBody func) $ do
            case (ASTResolved.funcRetty func) of
                Retty _    -> addFuncRefType symbol False
                RefRetty _ -> addFuncRefType symbol True
                _ -> return ()

            crt <- cRettyType (ASTResolved.funcRetty func) 
            cats <- forM (ASTResolved.funcArgs func) $ \param -> case param of
                S.Param _ _ _ -> cTypeOf param
                S.RefParam _ _ _ -> cRefTypeOf param
                x -> error (show x)

            newExtern (show symbol) crt cats

    -- generate functions, main is a special case
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
        unless (isGenericBody func) $ do
            generateFunc symbol func

            when (sym symbol == "main") $ do
                let ioTypedef = Type.TypeApply (SymResolved "io" "Io" 0) []
                id <- newFunction Cint "main" $ 
                    [ C.Param "argc" Cint, C.Param "argv" (Cpointer (Cpointer Cchar)) ]

                withCurID id $ do
                    appendElem $ C.ExprStmt $ C.Call "doodad_set_args" [C.Ident "argc", C.Ident "argv"]
                    call (show symbol) []
                    void $ appendElem $ C.Return $ C.Int 0
                withCurID globalID (append id)

cRettyType :: S.Retty -> Generate C.Type
cRettyType retty = case retty of
    VoidRetty -> return Cvoid
    Retty t -> cTypeOf t
    RefRetty t -> cRefTypeOf t


generateFunc :: Symbol -> FuncBody -> Generate ()
generateFunc symbol body = do
    args <- mapM cParamOf (ASTResolved.funcArgs body)
    rettyType <- cRettyType (ASTResolved.funcRetty body)
    pushSymTab

    id <- newFunction rettyType (show symbol) ([] ++ args)
    withCurID id $ do
        forM_ (ASTResolved.funcArgs body) $ \arg -> do
            let name = show (paramName arg)
            case arg of
                S.Param _ _ _ ->    define name $ Value (typeof arg) (C.Ident name)
                S.RefParam _ _ _ -> define name $ Ref (typeof arg) (C.Ident name)
                x -> error (show x)

        generateStmt (ASTResolved.funcStmt body)
        when (ASTResolved.funcRetty body /= S.VoidRetty) $ -- check to ensure function has return
            call "assert" [false]

    popSymTab
    withCurID globalID $ append id


generatePrint :: String -> Value -> Generate ()
generatePrint app val@(Value _ _) = case typeof val of
    Type.I64 ->    void $ appendPrintf ("%lld" ++ app) [valExpr val]
    Type.I32 ->    void $ appendPrintf ("%ld" ++ app) [valExpr val]
    Type.F64 ->    void $ appendPrintf ("%f" ++ app) [valExpr val]
    Type.F32 ->    void $ appendPrintf ("%f" ++ app) [valExpr val]
    Type.String -> void $ appendPrintf ("\"%s\"" ++ app) [valExpr val]
    Type.Char ->   void $ appendPrintf ("%c" ++ app) [valExpr val]

    Type.Bool -> void $ appendPrintf ("%s" ++ app) $
        [C.CndExpr (valExpr val) (C.String "true") (C.String "false")]

    Type.TypeApply (Sym "Tuple") ts -> do
        void $ appendPrintf "(" []
        forM_ (zip ts [0..]) $ \(t, i) -> do
            let ap = if i == (length ts - 1) then ")" ++ app else ", "
            generatePrint ap $ Value t $ C.Member (valExpr val) ("m" ++ show i)

    Type.TypeApply s t -> do
        base <- baseTypeOf val
        generatePrint app $ Value base (valExpr val)

    Type.TypeApply (Sym "Sum") ts -> do -- TODO
        void $ appendPrintf ("Sum" ++ app) []

    _ -> error (show $ typeof val)


generateStmt :: S.Stmt -> Generate ()
generateStmt stmt = withPos stmt $ case stmt of
    S.Block stmts          -> mapM_ generateStmt stmts
    S.EmbedC _ str         -> void $ appendElem (C.Embed str)

    S.Return _ Nothing     -> void $ appendElem (C.ReturnVoid)

    S.Return _ (Just expr) -> do
        val <- generateExpr expr
        case val of
            Value _ _ -> void $ appendElem $ C.Return (valExpr val)
            Ref _ _   -> void $ appendElem $ C.Return (refExpr val)


    S.ExprStmt expr -> void $ generateExpr expr
    S.Let _ pattern mexpr mblk -> do
        rhs <- case mexpr of
            Just expr -> generateExpr expr
            Nothing   -> initialiser (typeof pattern) []
        matched <- generatePattern pattern rhs
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

    S.SetOp _ S.Eq index expr -> do
        idx <- generateExpr index
        set idx =<< generateExpr expr

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
                Type.String -> return ()
                Type.TypeApply (Sym "Table") _ -> return ()
                x -> error (show x)


            -- check that index is still in range
            idxGtEq <- case base of
                Type.String                     -> generateInfix S.GTEq idx =<< len val
                Type.TypeApply (Sym "Table") _  -> generateInfix S.GTEq idx =<< len val
            if_ idxGtEq (appendElem C.Break)

            -- check that pattern matches
            patMatches <- case mpat of
                Nothing -> return true
                Just pat -> case base of
                    Type.TypeApply (Sym "Table") ts -> generatePattern pat =<< builtinTableAt val idx
                    _ -> error (show base)

            if_ (not_ patMatches) $ appendElem C.Break
            generateStmt stmt

    _ -> fail $ "invalid statement: " ++ (show stmt)



generatePattern :: Pattern -> Value -> Generate Value
generatePattern pattern val = withPos pattern $ do
    case pattern of
        PatIgnore _ -> return true
        PatLiteral expr -> generateInfix S.EqEq val =<< generateExpr expr
        PatAnnotated pat typ -> generatePattern pat val

        PatIdent _ symbol -> do 
            base <- baseTypeOf val
            let name = show symbol
            define name (Value (typeof val) $ C.Ident name)
            cType <- cTypeOf (typeof val)
            void $ appendAssign cType (show symbol) $ C.Initialiser [C.Int 0]
            set (Value (typeof val) $ C.Ident name) val
            return true

        PatTuple _ pats -> do
            base@(Type.TypeApply (Sym "Tuple") ts) <- baseTypeOf val
            unless (length pats == length ts) (error "invalid tuple pattern")

            match <- assign "match" false
            endLabel <- fresh "skipMatch"
            -- TODO early exit?
            forM_ (zip ts [0..]) $ \(t, i) -> do
                let v = Value t $ C.Member (valExpr val) ("m" ++ show i)
                b <- generatePattern (pats !! i) v
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

        PatField pos fieldType pats -> do
            TypeApply (Sym "Sum") ts <- baseTypeOf val
            let Just i = elemIndex fieldType ts

            endLabel <- fresh "skipMatch"
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


-- generateExpr should return a 're-enter-able' expression, eg 1, not func()
generateExpr :: Expr -> Generate Value
generateExpr (AExpr typ expr_) = withPos expr_ $ withTypeCheck $ case expr_ of
    S.Bool _ b             -> return $ Value typ (C.Bool b)
    S.Int _ n              -> return $ Value typ (C.Int n)
    S.Float _ f            -> return $ Value typ (C.Float f)
    S.String _ s           -> return $ Value typ (C.String s)
    S.Char _ c             -> return $ Value typ (C.Char c)
    S.Match _ expr pattern -> generatePattern pattern =<< generateExpr expr
    S.Ident _ symbol       -> do
        val <- look (show symbol)
        case val of
            Value _ _ -> return val
            Ref   t e -> return val

    S.Builtin _ "conv" [expr] -> convert typ =<< generateExpr expr

    S.Builtin _ "print" exprs -> do
        vals <- mapM generateExpr exprs
        forM_ (zip vals [0..]) $ \(val, i) -> do
            let end = i == length vals - 1
            generatePrint (if end then "\n" else ", ") val
        return $ Value Void $ C.Int 0

    S.Builtin pos "assert" [cnd, str] -> do
        cndVal <- generateExpr cnd
        strVal <- generateExpr str
        fileNameVal <- return $ Value Type.String (C.String $ textFile pos)
        lineVal <- return $ Value I64 (C.Int $ fromIntegral $ textLine pos)
        call "doodad_assert" [fileNameVal, lineVal, cndVal, strVal]
        return $ Value Void $ C.Int 0

    S.Builtin _ "builtin_table_at" [expr1, expr2] -> do
        val <- generateExpr expr1
        idx <- generateExpr expr2
        Type.TypeApply (Sym "Table") _ <- baseTypeOf val
        builtinTableAt val idx


    S.Builtin _ "builtin_table_append" [expr1] -> do
        val <- generateExpr expr1
        TypeApply (Sym "Table") _ <- baseTypeOf val
        case val of
            Value _ _ -> fail "isn't reference"
            Ref _ _   -> tableAppend val >> return (Value Void $ C.Int 0)


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

    S.Call _ Nothing symbol exprs -> do
        check (symbolIsResolved symbol) ("unresolved function call: " ++ show symbol)
        vals <- mapM generateExpr exprs
        argExprs <- forM vals $ \val -> case val of
            Value _ _ -> return (valExpr val)
            Ref _ _   -> return (refExpr val)
        case typ of
            Void -> do
                appendElem $ C.ExprStmt $ C.Call (show symbol) argExprs
                return $ Value Void $ C.Int 0
            _ -> do
                isRef <- getFuncRefType symbol
                if isRef then assign "call" $ Ref typ $ C.Call (show symbol) argExprs
                else          assign "call" $ Value typ $ C.Call (show symbol) argExprs

    S.Field _ expr idx -> do 
        member idx =<< generateExpr expr

    S.Tuple _ exprs -> do
        vals <- mapM generateExpr exprs
        base <- baseTypeOf typ
        case base of
            Type.TypeApply (Sym "Tuple") ts -> do
                unless (length ts == length vals) (error "invalid tuple type")
                initialiser typ vals
            _ -> error (show base)

    S.Construct pos typ exprs -> do
        vals <- mapM generateExpr exprs
        case vals of
            [val] -> convert typ val
            []    -> initialiser typ []

    S.Reference pos expr -> do
        val <- generateExpr expr
        base <- baseTypeOf val
        case val of
            Ref _ _ -> return val
            Value _ _ -> case base of
                x | isSimple x                  -> return $ Ref (typeof val) (C.Address $ valExpr val)
                TypeApply (Sym "Sum") _         -> return $ Ref (typeof val) (C.Address $ valExpr val)
                TypeApply (Sym "Table") _       -> return $ Ref (typeof val) (C.Address $ valExpr val)
                Type.TypeApply (Sym "Tuple") ts -> do
                    ref <- assign "ref" $ Ref (typeof val) $ C.Initialiser [C.Address (valExpr val), C.Int 0, C.Int 0]
                    return ref
                x -> error (show x)

    _ -> error (show expr_)
    where
        withTypeCheck :: Generate Value -> Generate Value
        withTypeCheck f = do
            r <- f
            unless (typeof r == typ) $ error ("generateExpr returned incorrect type: " ++ show (typeof r))
            return r
generateExpr x = fail $ "unresolved expression: " ++ show x
            

generateInfix :: S.Operator -> Value -> Value -> Generate Value
generateInfix op a' b' = do
    a <- deref a'
    b <- deref b'

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


        Type.TypeApply (Sym "Tuple") ts -> do
            res <- assign "res" =<< case op of
                S.Plus -> initialiser (typeof a) []
                _      -> return true

            withFakeSwitch $ do
                forM_ (zip ts [0..]) $ \(t, i) -> do
                    let ma = Value t $ C.Member (valExpr a) ("m" ++ show i)
                    let mb = Value t $ C.Member (valExpr b) ("m" ++ show i)
                    eq <- generateInfix S.EqEq ma mb
                    case op of
                        S.NotEq -> void $ if_ (not_ eq) (appendElem C.Break)
                        S.EqEq -> if_ (not_ eq) $ do
                            set res false
                            void $ appendElem C.Break

                        S.LT -> do
                            lt <- generateInfix S.LT ma mb
                            if_ lt (appendElem C.Break)
                            void $ if_ (not_ eq) $ do
                                set res false
                                appendElem C.Break

                        S.GT -> do
                            gt <- generateInfix S.GT ma mb
                            if_ gt (appendElem C.Break)
                            void $ if_ (not_ eq) $ do
                                set res false
                                appendElem C.Break

                        S.Plus -> do
                            let mr = Value t $ C.Member (valExpr res) ("m" ++ show i)
                            void $ set mr =<< generateInfix S.Plus ma mb

                case op of
                    S.NotEq -> set res false
                    S.LT    -> set res false
                    S.GT    -> set res false
                    _       -> return ()

            return res
            
        _ -> error $ show (base, op)
