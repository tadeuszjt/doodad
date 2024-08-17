module Compile where

import Data.Maybe
import Data.Char
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
import qualified MakeFuncIR as IR
import qualified IR


generateAst :: MonadIO m => ASTResolved -> m (Either Error (((), GenerateState), BuilderState))
generateAst ast = runGenerate (initGenerateState ast) C.initBuilderState generate


generate :: Generate ()
generate = withErrorPrefix "generate: " $ do
    ast <- gets astResolved

    -- generate imported function externs
    forM_ (Map.toList $ funcInstance ast) $ \(funcType, header) -> do
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
                    (Type.Func, mainRetType : mainArgTypes) <- unfoldType <$> baseTypeOf (TypeDef symbol)

                    cts <- mapM cTypeOf mainArgTypes
                    vals <- forM mainArgTypes $ \argType -> do
                        assign "mainArg" $ Value argType $ C.Initialiser [C.Int 0]

                    args <- forM (zip vals (S.funcArgs $ funcHeader func)) $ \(val, param) -> case param of
                        S.Param _ _ _ -> return val
                        x -> error (show x)

                    call (showSymGlobal $ funcSymbol header) args
                    void $ appendElem $ C.Return $ C.Int 0
                withCurID globalID (append id)


cRettyType :: S.Retty -> Generate C.Type
cRettyType retty = case retty of
    Retty t -> cTypeOf t
    RefRetty t -> cRefTypeOf t


generateFunc :: Type.Type -> Generate FuncHeader
generateFunc funcType = do
    let (TypeDef symbol, typeArgs) = unfoldType funcType

    isInstance <- gets (Map.member funcType . funcInstance . astResolved)
    isFunction <- gets (Map.member symbol . funcDefsAll . astResolved)
    isAcquire  <- gets (Map.member symbol . featuresAll . astResolved)

    if isInstance then
        fromJust <$> gets (Map.lookup funcType . funcInstance . astResolved)
    else do
        ast <- gets astResolved
        funcAst <- fmap fst $ runDoMExcept ast (makeInstance funcType)
        func <- fmap fst $ runDoMExcept (IR.initFuncIRState ast) (IR.makeFuncIR funcAst)

        liftIO $ IR.prettyIR "" func

        symbol <- CGenerate.genSymbol (funcSymbol $ IR.irHeader func)
        let header' = (IR.irHeader func) { funcSymbol = symbol }


        modify $ \s -> s { astResolved = (astResolved s)
            { funcInstance = Map.insert funcType header' (funcInstance $ astResolved s) } }

        pushSymTab
        --args <- mapM cParamOf (S.funcArgs header')
        args <- forM (IR.irArgs func) $ \arg -> case arg of
            IR.ParamIR (IR.ArgID id) IR.Value typ -> do
                cType <- cTypeOf typ
                return $ C.Param (idName id) cType

            IR.ParamIR (IR.ArgID id) IR.Ref typ -> do
                cType <- cRefTypeOf typ
                return $ C.Param (idName id) cType

            x -> error (show x)


        rettyType <- cRettyType (S.funcRetty header')

        funcId <- newFunction rettyType (showSymGlobal $ symbol) ([] ++ args) []
        withCurID funcId $ do
            forM_ (IR.irArgs func) $ \arg -> do
                --error (show arg)
                return ()

            generateStmt func 0


--            forM_ (S.funcArgs header') $ \arg -> do
--                let name = showSymLocal (paramSymbol arg)
--                case arg of
--                    S.Param _ _ _ ->    define name $ Value (typeof arg) (C.Ident name)
--                    S.RefParam _ _ _ -> define name $ Ref (typeof arg) (C.Ident name)
--                    x -> error (show x)
--
--
--            isRefRetty <- case S.funcRetty header' of
--                RefRetty _ -> return True
--                Retty _    -> return False
--
--            oldCurFnIsRef <- gets curFnIsRef
--            modify $ \s -> s { curFnIsRef = isRefRetty }
--            generateStmt (IR.irStatement func)
--            modify $ \s -> s { curFnIsRef = oldCurFnIsRef }
--
--            when (S.funcRetty header' /= S.Retty Void) $ -- check to ensure function has return
--                call "assert" [false]

        popSymTab
        withCurID globalID (append funcId)

        return header'


idName :: IR.ID -> String
idName x = "_" ++ show x



processCEmbed :: String -> Generate String
processCEmbed str = case str of
    ('$':'%':xs) -> do
        let num = takeWhile (\c -> isDigit c) xs
        check (length num > 0)     "invalid identifier following '$' token"
        let rest = drop (length num) xs

        (("_" ++ num) ++) <$> processCEmbed rest

    (x:xs) -> (x:) <$> processCEmbed xs
    []     -> return ""


generateArg :: IR.Arg -> Generate C.Expression
generateArg (IR.ArgID id) = return $ C.Ident (idName id)
generateArg (IR.ArgConst typ const) = case const of
    IR.ConstBool b -> return (C.Bool b)
    IR.ConstChar c -> return (C.Char c)
    IR.ConstInt  n -> return (C.Int n)
    IR.ConstString s -> return (C.String s)
    x -> error (show x)


generateStmt :: IR.FuncIR -> IR.ID -> Generate ()
generateStmt funcIr id = case (IR.irStmts funcIr) Map.! id of
    IR.Block ids -> do
        mapM_ (generateStmt funcIr) ids

    IR.EmbedC _ str  -> do
        str' <- processCEmbed str
        void $ appendElem (C.Embed str')

    IR.InitVar marg -> do
        let (typ, IR.Value) = (IR.irTypes funcIr) Map.! id

        cType <- cTypeOf typ

        cexpr <- case marg of
            Nothing -> return (C.Initialiser [C.Int 0])
            Just arg -> generateArg arg

        void $ appendAssign cType (idName id) cexpr

    IR.MakeString str -> do
        let (Apply Slice Type.Char, IR.Value) = (IR.irTypes funcIr) Map.! id
        cType <- cTypeOf (Apply Slice Type.Char)
        let len = fromIntegral (length str)
        void $ appendAssign cType (idName id) $ C.Initialiser [C.String str, C.Int len, C.Int len]

    IR.Return arg -> void $ appendElem . C.Return =<< generateArg arg

    IR.ReturnVoid -> void $ appendElem C.ReturnVoid

    IR.MakeReferenceFromValue argId -> case (IR.irTypes funcIr) Map.! argId of
        (typ, IR.Value) -> do
            cexpr <- generateArg (IR.ArgID argId)
            base <- baseTypeOf typ
            case unfoldType base of
                (Tuple, ts) -> do
                    cRefType <- cRefTypeOf typ
                    void $ appendAssign cRefType (idName id) $ C.Initialiser [C.Address cexpr, C.Int 0, C.Int 1]
                (_, _) -> do
                    cRefType <- cRefTypeOf typ
                    void $ appendAssign cRefType (idName id) (C.Address cexpr)

        x -> error (show x)

    IR.Call callType args -> do
        let (TypeDef funcSymbol, _) = unfoldType callType
        header <- generateFunc callType
        cArgs <- mapM generateArg args
        let cCall = C.Call (showSymGlobal $ S.funcSymbol header) cArgs

        case (IR.irTypes funcIr) Map.! id of
            (Void, IR.Const) -> void $ appendElem (C.ExprStmt cCall)

            (typ, IR.Value) -> do
                cType <- cTypeOf typ
                void $ appendAssign cType (idName id) cCall

            (typ, IR.Ref) -> do
                cRefType <- cRefTypeOf typ
                void $ appendAssign cRefType (idName id) cCall


            x -> error (show x)

    IR.Loop ids -> do
        forId <- appendElem $ C.For Nothing Nothing Nothing []
        withCurID forId $ do
            mapM_ (generateStmt funcIr) ids


    IR.If arg ids -> do
        val <- generateArg arg
        ifId <- appendElem (C.If val [])
        withCurID ifId $ do
            mapM_ (generateStmt funcIr) ids
        
    IR.Else ids -> do
        elseId <- appendElem (C.Else [])
        withCurID elseId $ do
            mapM_ (generateStmt funcIr) ids

    IR.Break -> void $ appendElem $ C.Break

    IR.MakeValueFromReference argId -> case (IR.irTypes funcIr) Map.! argId of
        (typ, IR.Ref) -> do
            cType <- cTypeOf typ
            cexpr <- generateArg (IR.ArgID argId)
            Value _ cRef <- deref (Ref typ cexpr)
            void $ appendElem $ C.Assign cType (idName id) cRef

        x -> error (show x)

    IR.MakeFieldFromVal argId i -> do
        let (typ, IR.Value) = (IR.irTypes funcIr) Map.! argId
        cexpr <- generateArg (IR.ArgID argId)
        base <- baseTypeOf typ

        case (IR.irTypes funcIr) Map.! id of
            (t, IR.Value) -> do
                cType <- cTypeOf t
                case unfoldType base of
                    (Tuple, ts) -> void $ appendAssign cType (idName id) (C.Member cexpr $ "m" ++ show i)
                    (Sum, ts)   -> void $ appendAssign cType (idName id) (C.Member cexpr $ "u" ++ show i)

                    x -> error (show x)
            x -> error (show x)


    IR.MakeFieldFromRef argId i -> do
        let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId
        cexpr <- generateArg (IR.ArgID argId)
        cType <- cTypeOf argType
        cRefType <- cRefTypeOf argType
        base <- baseTypeOf argType

        case (IR.irTypes funcIr) Map.! id of
            (_, IR.Value) -> case unfoldType base of
                --(Sum, ts)   -> return $ Value (ts !! i) $ C.PMember cexpr ("u" ++ show i)
                (Tuple, ts) -> do
                    cts <- mapM cTypeOf ts
                    ct <- cTypeOf base
                    let off = C.Offsetof ct ("m" ++ show i)
                    let ptr = C.Cast (Cpointer Cvoid) (C.Member cexpr "ptr")
                    let idx = C.Member cexpr "idx"
                    let cap = C.Member cexpr "cap"

                    cTypeField <- cTypeOf (ts !! i)

                    void $ appendAssign cTypeField (idName id) $ C.Deref $
                        C.Cast (Cpointer cTypeField) $ C.Infix C.Plus ptr $ C.Infix Plus
                            (C.Infix C.Times cap off) (C.Infix C.Times idx $ C.SizeofType cTypeField)


            (_, IR.Ref) -> case unfoldType base of
                (Tuple, ts) -> do
                    cts <- mapM cTypeOf ts
                    ct <- cTypeOf base
                    let off = C.Offsetof ct ("m" ++ show i)
                    let ptr = C.Cast (Cpointer Cvoid) (C.Member cexpr "ptr")
                    let idx = C.Member cexpr "idx"
                    let cap = C.Member cexpr "cap"
                    let ptr' = C.Cast (Cpointer $ cts !! i) $ 
                            C.Infix C.Plus ptr $ C.Infix Plus
                                (C.Infix C.Times cap off) (C.Infix C.Times idx $ C.SizeofType $ cts !! i)

                    baseField <- baseTypeOf (ts !! i)
                    cRefTypeField <- cRefTypeOf (ts !! i)
                    case unfoldType baseField of
                        (Tuple, _) -> do
                            void $ appendAssign cRefTypeField (idName id) $ C.Initialiser [ptr', C.Int 0, C.Int 1]
                        (_, _) -> do
                            void $ appendAssign cRefTypeField (idName id) ptr'

                        x -> error (show x)


                (Sum, ts) -> do
                    ct <- cTypeOf (ts !! i)
                    let ptr = C.Address (C.PMember cexpr $ "u" ++ show i)
                    base <- baseTypeOf (ts !! i)
                    cRefType <- cRefTypeOf (ts !! i)
                    case unfoldType base of
                        (Type.Tuple, ts) -> do
                            void $ appendAssign cRefType (idName id) $ C.Initialiser [ptr, C.Int 0, C.Int 1]


                        x -> error (show x)

                    --void $ appendAssign cRefType (idName id) $ C.Initialiser [ptr', C.Int 0, C.Int 1]





                x -> error (show x)


                --(Table, [t]) -> do
                --    baseT <- baseTypeOf t
                --    case unfoldType baseT of
                --        (Tuple, ts) -> do
                --            ct <- cTypeOf baseT
                --            let off = C.Offsetof ct ("m" ++ show i)
                --            let ptr = C.Cast (Cpointer Cvoid) (C.PMember expr "r0")
                --            let row = C.Infix C.Plus ptr $ C.Infix C.Times off $ C.PMember expr "cap"
                --            -- setting slice cap to 0 represents non-flat memory
                --            assign "slice" $ Value (Apply Slice $ ts !! i) $
                --                C.Initialiser [ row, C.PMember expr "len", C.Int 0 ]

            x -> error (show x)

        

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
        C.Initialiser [C.String s, C.Int (fromIntegral $ length s), C.Int (fromIntegral $ length s)]

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
                assign "ref" (Ref typ refExpr)

            (s, exprs) -> do
                args <- mapM generateExpr exprs
                header <- generateFunc funcType
                let symbol = funcSymbol header
            
                argExprs <- forM (zip args $ S.funcArgs header) $ \(arg, param) -> case (arg, param) of
                    (Value _ valType, S.Param _ _ paramType) -> valExpr <$> convert paramType arg
                    (Ref _ _, S.RefParam _ _ paramType)      -> refExpr <$> convert paramType arg
                    (Ref _ _, S.Param _ _ paramType)         -> valExpr <$> (convert paramType =<< deref arg)
                    (Value _ _, S.RefParam _ _ paramType)    -> refExpr <$> (convert paramType =<< makeRef arg)
                    x -> error (show x)

                case S.funcRetty header of
                    Retty Void -> do
                        appendElem $ C.ExprStmt $ C.Call (showSymGlobal symbol) argExprs
                        return $ Value Void (C.Int 0)
                    Retty retType    -> assign "call" $ Value typ $ C.Call (showSymGlobal symbol) argExprs
                    RefRetty retType -> assign "call" $ Ref typ $ C.Call (showSymGlobal symbol) argExprs

    S.Field _ expr field -> do
        i <- case field of
            Left i -> return i
            Right symbol -> gets (fst . fromJust . Map.lookup symbol . fieldsAll . astResolved)

        val <- generateExpr expr
        base <- baseTypeOf val
        case val of
            Value _ expr -> case unfoldType base of
                (Tuple, ts) -> return $ Value (ts !! i) $ C.Member expr ("m" ++ show i)
                (Sum, ts)   -> return $ Value (ts !! i) $ C.Member expr ("u" ++ show i)
                (Table, [t]) -> do
                    baseT <- baseTypeOf t
                    case unfoldType baseT of
                        (Tuple, ts) -> do
                            ct <- cTypeOf baseT
                            let off = C.Offsetof ct ("m" ++ show i)
                            let ptr = C.Cast (Cpointer Cvoid) (C.Member expr "r0")
                            let row = C.Infix C.Plus ptr (C.Infix C.Times off $ C.Member expr "cap")

                            -- setting slice cap to 0 represents non-flat memory
                            assign "slice" $ Value (Apply Slice $ ts !! i) $
                                C.Initialiser [ row, C.Member expr "len", C.Int 0 ]

                x -> error (show x)

            Ref _ expr -> case unfoldType base of
                (Sum, ts)   -> return $ Value (ts !! i) $ C.PMember (refExpr val) ("u" ++ show i)
                (Tuple, ts) -> do
                    cts <- mapM cTypeOf ts
                    ct <- cTypeOf base
                    let off = C.Offsetof ct ("m" ++ show i)
                    let ptr = C.Cast (Cpointer Cvoid) (C.Member expr "ptr")
                    let idx = C.Member expr "idx"
                    let cap = C.Member expr "cap"
                    return $ Value (ts !! i) $ C.Deref $ C.Cast (Cpointer $ cts !! i) $ 
                        C.Infix C.Plus ptr $ C.Infix Plus
                            (C.Infix C.Times cap off) (C.Infix C.Times idx $ C.SizeofType $ cts !! i)

                (Table, [t]) -> do
                    baseT <- baseTypeOf t
                    case unfoldType baseT of
                        (Tuple, ts) -> do
                            ct <- cTypeOf baseT
                            let off = C.Offsetof ct ("m" ++ show i)
                            let ptr = C.Cast (Cpointer Cvoid) (C.PMember expr "r0")
                            let row = C.Infix C.Plus ptr $ C.Infix C.Times off $ C.PMember expr "cap"
                            -- setting slice cap to 0 represents non-flat memory
                            assign "slice" $ Value (Apply Slice $ ts !! i) $
                                C.Initialiser [ row, C.PMember expr "len", C.Int 0 ]

                x -> error (show x)
                        
    S.Reference pos expr -> makeRef =<< generateExpr expr

    S.Array pos exprs -> do
        vals <- mapM deref =<< mapM generateExpr exprs
        cTyp <- cTypeOf (head vals)
        name <- fresh "array"
        appendElem $ C.Assign (Carray (length exprs) cTyp) name $ C.Initialiser (map valExpr vals)
        assign "slice" $ Value typ $ C.Initialiser
            [ C.Ident name , C.Int (fromIntegral $ length exprs) , C.Int 0 ]

    _ -> error (show expr_)
    where
        withTypeCheck :: Generate Value -> Generate Value
        withTypeCheck f = do
            r <- f
            unless (typeof r == typ) $ error ("generateExpr returned incorrect type: " ++ show (typeof r))
            return r
generateExpr x = fail $ "unresolved expression: " ++ show x
