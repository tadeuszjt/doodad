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
import qualified FuncIrDestroy as IR
import qualified FuncIrUnused as IrUnused
import qualified FuncIrInline as IrInline
import qualified FuncIrConst as IrConst
import qualified FuncIrChecker as IrChecker


generateAst :: MonadIO m => ASTResolved -> m (Either Error (((), GenerateState), BuilderState))
generateAst ast = runGenerate (initGenerateState ast) C.initBuilderState generate


generate :: Generate ()
generate = withErrorPrefix "generate: " $ do
    ast <- gets astResolved

    -- generate imported function externs
    forM_ (Map.toList $ funcInstance ast) $ \(funcType, (header, _)) -> do
        crt <- case IR.irRetty header of
            IR.RettyIR IR.Value t -> cTypeOf t
            IR.RettyIR IR.Ref t -> cRefTypeOf t

        cats <- forM (IR.irArgs header) $ \param -> case param of
            IR.ParamIR IR.Value t -> cTypeOf t
            IR.ParamIR IR.Ref t -> cRefTypeOf t
        appendExtern (showSymGlobal $ IR.irFuncSymbol header) crt cats [C.Extern]


    forM_ (featuresTop ast) $ \symbol -> let Just acq = Map.lookup symbol (featuresAll ast) in do
        let Just (generics, _) = Map.lookup symbol (typeDefsAll ast)
        when (generics == []) $ do
            header <- generateFunc (TypeDef symbol)
            when (symbolsCouldMatch (Sym ["main"]) (IR.irFuncSymbol header)) $ do
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

                    args <- forM (zip vals (IR.irArgs header)) $ \(val, param) -> case param of
                        IR.ParamIR IR.Value _ -> return val
                        x -> error (show x)

                    void $ appendElem $ C.ExprStmt $ C.Call
                        (showSymGlobal $ IR.irFuncSymbol header)
                        (map valExpr args)

                    void $ appendElem $ C.Return $ C.Int 0
                withCurID globalID (append id)



generateFunc :: Type.Type -> Generate IR.FuncIrHeader
generateFunc funcType = do
    let (TypeDef symbol, typeArgs) = unfoldType funcType
    isInstance <- gets (Map.member funcType . funcInstance . astResolved)
    if isInstance then gets $ fst . fromJust . Map.lookup funcType . funcInstance . astResolved
    else do
        ast <- gets astResolved
        Just funcAst <- fmap fst $ runDoMExcept ast (makeInstance funcType)
        (funcIrHeader, funcIr') <- fmap fst $ runDoMExcept (IR.initFuncIRState ast) (IR.makeFuncIR funcAst)

        void $ runDoMExcept (IrChecker.initFuncIrCheckerState ast) (IrChecker.funcIrChecker funcIr')



        funcIr'' <- fmap (IR.funcIr . snd) $ runDoMExcept (IR.initFuncIrDestroyState ast) (IR.addFuncDestroy funcIr')
        funcIr''' <- fmap (IrInline.funcIr . snd) $ runDoMExcept (IrInline.initFuncIrInlineState ast) (IrInline.funcIrInline funcIr'')

        ((funcIr, n), _) <- runDoMExcept () $ runDoMUntilSameResult funcIr''' $ \funcIr -> do
            funcIr' <- fmap (IrUnused.funcIr . snd) $ runDoMExcept (IrUnused.initFuncIrUnusedState ast) (IrUnused.funcIrUnused funcIr)
            funcIr'' <- fmap (IrInline.funcIr . snd) $ runDoMExcept (IrInline.initFuncIrInlineState ast) (IrInline.funcIrInline funcIr')
            funcIr''' <- fmap (IrConst.funcIr . snd) $ runDoMExcept (IrConst.initFuncIrConstState ast) (IrConst.funcIrConst funcIr'')
            return funcIr'''

        liftIO $ putStrLn (show n)
        liftIO $ putStrLn $ show funcIrHeader
        liftIO $ IR.prettyIR "" funcIr

        generatedSymbol <- CGenerate.genSymbol symbol
        --liftIO $ putStrLn $ "generating: " ++ prettySymbol generatedSymbol
        let header' = (funcIrHeader) { IR.irFuncSymbol = generatedSymbol }

        modify $ \s -> s { astResolved = (astResolved s)
            { funcInstance = Map.insert funcType (header', funcIr) (funcInstance $ astResolved s) } }

        args <- forM (zip (IR.irArgs funcIrHeader) [1..]) $ \(arg, i) -> case arg of
            IR.ParamIR IR.Value typ -> do
                cType <- cTypeOf typ
                return $ C.Param (idName i) cType

            IR.ParamIR IR.Ref typ -> do
                cType <- cRefTypeOf typ
                return $ C.Param (idName i) cType

            x -> error (show x)

        cRettyType <- case IR.irRetty funcIrHeader of
            IR.RettyIR IR.Value typ -> cTypeOf typ
            IR.RettyIR IR.Ref   typ -> cRefTypeOf typ
            x -> error (show x)

        appendExtern (showSymGlobal $ generatedSymbol) cRettyType (map C.cType args) []
        funcId <- newFunction cRettyType (showSymGlobal $ generatedSymbol) args []
        withCurID funcId $ do
            (generateStmt funcIr 0)

        withCurID globalID (append funcId)
        return header'


idName :: IR.ID -> String
idName x = "_" ++ show x



processCEmbed :: [(String, IR.ID)] -> String -> Generate String
processCEmbed strMap str = case str of
    ('$':x:xs) -> do
        unless (isAlpha x) (fail "invalid identifier following '$' token")
        let ident = takeWhile (\c -> isAlpha c || isDigit c) (x:xs)
        let rest = drop (length ident) (x:xs)

        num <- case lookup ident strMap of
            Just id -> return id
            Nothing -> fail ("invalid ident in C embed: " ++ ident)

        (("_" ++ show num) ++) <$> processCEmbed strMap rest

    (x:xs) -> (x:) <$> processCEmbed strMap xs
    []     -> return ""


generateArg :: IR.Arg -> Generate C.Expression
generateArg (IR.ArgID id) = return $ C.Ident (idName id)
generateArg (IR.ArgConst typ const) = case const of
    IR.ConstBool b -> return (C.Bool b)
    IR.ConstChar c -> return (C.Char c)
    IR.ConstInt  n -> return (C.Int n)
    IR.ConstString s -> return (C.String s)
    IR.ConstFloat f  -> return (C.Float f)
    x -> error (show x)


generateStmt :: IR.FuncIR -> IR.ID -> Generate ()
generateStmt funcIr id = case (IR.irStmts funcIr) Map.! id of
    IR.Break -> void $ appendElem $ C.Break
    IR.Block ids -> mapM_ (generateStmt funcIr) ids
    IR.EmbedC strMap str  -> void $ appendElem . C.Embed =<< processCEmbed strMap str
    IR.Return arg -> void $ appendElem . C.Return =<< generateArg arg
    IR.ReturnVoid -> void $ appendElem C.ReturnVoid

    IR.Loop ids -> do
        forId <- appendElem $ C.For Nothing Nothing Nothing []
        withCurID forId $ do
            mapM_ (generateStmt funcIr) ids


    IR.If arg trueBlkId falseBlkId -> do
        val <- generateArg arg
        ifId <- appendElem (C.If val [])
        elseId <- appendElem (C.Else [])
        withCurID ifId $ generateStmt funcIr trueBlkId
        withCurID elseId $ generateStmt funcIr falseBlkId
        
    IR.SSA operation -> let Just (ssaTyp, ssaRefTyp) = Map.lookup id (IR.irTypes funcIr) in
        case operation of
            IR.InitVar marg -> do
                let IR.Value = ssaRefTyp
                cType <- cTypeOf ssaTyp
                cexpr <- case marg of
                    Nothing -> return (C.Initialiser [C.Int 0])
                    Just arg -> generateArg arg

                void $ appendAssign cType (idName id) cexpr

            IR.MakeString str -> do
                let Apply Slice Type.Char = ssaTyp
                let IR.Value = ssaRefTyp
                cType <- cTypeOf ssaTyp
                let len = fromIntegral (length str)
                void $ appendAssign cType (idName id) $ C.Initialiser [C.String str, C.Int len, C.Int len]


            IR.MakeReferenceFromValue (IR.ArgID argId) -> case (IR.irTypes funcIr) Map.! argId of
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

                case funcSymbol of
                    x | symbolsCouldMatch x (Sym ["builtin", "builtinTableAppend"]) -> do
                        unless (length args == 1) (error "arg length mismatch")
                        let (IR.ArgID argId) = head args
                        let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId
                        cexpr <- generateArg (head args)
                        builtinTableAppend argType cexpr

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinStore"]) -> do
                        let (TypeDef _, [typ]) = unfoldType callType
                        [cexpr1, cexpr2] <- mapM generateArg args
                        base <- baseTypeOf typ
                        ct <- cTypeOf base
                        case unfoldType base of
                            (Tuple, ts) -> do
                                forM_ (zip ts [0..]) $ \(t, i) -> do
                                    ci <- cTypeOf t
                                    let off = C.Offsetof ct ("m" ++ show i)
                                    let cap = C.Member cexpr1 "cap"
                                    let idx = C.Member cexpr1 "idx"
                                    let ptr = C.Cast (Cpointer Cvoid) (C.Member cexpr1 "ptr")
                                    let size = C.SizeofType ci
                                    let offset = C.Infix C.Plus (C.Infix C.Times cap off) (C.Infix C.Times idx size)
                                    let fieldP = C.Cast (Cpointer ci) (C.Infix C.Plus ptr offset)

                                    void $ appendElem $ C.Set (C.Deref fieldP) $ C.Member cexpr2 ("m" ++ show i)

                            _ ->  void $ appendElem $ C.Set  (C.Deref cexpr1) cexpr2
                            x -> error (show x) 

                        --void $ appendElem $ C.Set (C.Deref cexpr1) cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinAdd"]) -> do
                        [cexpr1, cexpr2] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Infix C.Plus cexpr1 cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinSubtract"]) -> do
                        [cexpr1, cexpr2] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Infix C.Minus cexpr1 cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinMultiply"]) -> do
                        [cexpr1, cexpr2] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Infix C.Times cexpr1 cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinDivide"]) -> do
                        [cexpr1, cexpr2] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Infix C.Divide cexpr1 cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinModulo"]) -> do
                        [cexpr1, cexpr2] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Infix C.Modulo cexpr1 cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinEqual"]) -> do
                        [cexpr1, cexpr2] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Infix C.EqEq cexpr1 cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinLessThan"]) -> do
                        [cexpr1, cexpr2] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Infix C.LT cexpr1 cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinGreaterThan"]) -> do
                        [cexpr1, cexpr2] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Infix C.GT cexpr1 cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinAnd"]) -> do
                        [cexpr1, cexpr2] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Infix C.AndAnd cexpr1 cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinOr"]) -> do
                        [cexpr1, cexpr2] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Infix C.OrOr cexpr1 cexpr2

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinNot"]) -> do
                        [cexpr1] <- mapM generateArg args
                        cType <- cTypeOf ssaTyp
                        void $ appendAssign cType (idName id) $ C.Not cexpr1

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinConvert"]) -> do
                        unless (length args == 1) (error "arg length mismatch")
                        --let (IR.ArgID argId) = head args

                        argType <- case head args of
                            IR.ArgID argId -> return $ fst $ (IR.irTypes funcIr) Map.! argId
                            IR.ArgConst typ _ -> return typ

                        cexpr <- generateArg (head args)
                        cType <- cTypeOf argType
                        void $ appendAssign cType (idName id) cexpr

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinTableLen"]) -> do
                        [cexpr] <- mapM generateArg args
                        void $ appendAssign C.Cint64_t (idName id) (C.PMember cexpr "len")

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinArrayLen"]) -> do
                        unless (length args == 1) (error "arg length mismatch")
                        let (IR.ArgID argId) = (args !! 0)
                        let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId
                        base <- baseTypeOf argType
                        case unfoldType base of
                            (Type.Array, [Size n, t]) -> do
                                void $ appendAssign C.Cint64_t (idName id) (C.Int $ fromIntegral n)
                            x -> error (show x)

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinSumReset"]) -> do
                        unless (length args == 2) (error "arg length mismatch")
                        let (IR.ArgID argId) = (args !! 0)
                        let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId

                        (Sum, _) <- unfoldType <$> baseTypeOf argType
                        cref <- generateArg (args !! 0)
                        cidx <- generateArg (args !! 1)

                        appendElem $ C.ExprStmt $ C.Call "memset" [ cref , C.Int 0, C.Sizeof (C.Deref $ cref) ]
                        appendElem $ C.Set (C.PMember cref "en") cidx
                        return ()

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinSumEnum"]) -> do
                        [cexpr] <- mapM generateArg args
                        void $ appendAssign C.Cint64_t (idName id) $ C.PMember cexpr "en"

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinSliceLen"]) -> do
                        [cexpr] <- mapM generateArg args
                        void $ appendAssign C.Cint64_t (idName id) $ C.Member cexpr "len"

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinPretend"]) -> do
                        [cexpr] <- mapM generateArg args
                        cRefType <- cRefTypeOf ssaTyp
                        void $ appendAssign cRefType (idName id) cexpr

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinArrayAt"]) -> do
                        [carg, cidx] <- mapM generateArg args

                        let IR.ArgID argId = args !! 0
                        let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId
                        Ref refTyp cref <- builtinArrayAt argType carg cidx

                        case (IR.irTypes funcIr) Map.! id of
                            (_, IR.Ref) -> do
                                cRefType <- cRefTypeOf refTyp
                                void $ appendAssign cRefType (idName id) cref

                            x -> error (show x)

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinTableAt"]) -> do
                        unless (length args == 2) (error "arg length mismatch")

                        carg <- generateArg (args !! 0)
                        cidx <- generateArg (args !! 1)

                        let IR.ArgID argId = args !! 0

                        let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId
                        Ref refTyp cref <- builtinTableAt argType carg cidx
                        case (IR.irTypes funcIr) Map.! id of
                            (_, IR.Ref) -> do
                                cRefType <- cRefTypeOf refTyp
                                void $ appendAssign cRefType (idName id) cref

                            x -> error (show x)

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinSliceAt"]) -> do
                        unless (length args == 2) (error "arg length mismatch")

                        carg <- generateArg (args !! 0)
                        cidx <- generateArg (args !! 1)

                        let IR.ArgID argId = args !! 0

                        let (argType, IR.Value) = (IR.irTypes funcIr) Map.! argId
                        Ref refTyp cref <- builtinSliceAt argType carg cidx

                        case (IR.irTypes funcIr) Map.! id of
                            (_, IR.Ref) -> do
                                cRefType <- cRefTypeOf refTyp
                                void $ appendAssign cRefType (idName id) cref

                            x -> error (show x)

                    x | symbolsCouldMatch x (Sym ["builtin", "builtinField"]) -> do
                        unless (length args == 1) (error "arg length mismatch")
                        let (TypeDef _, [Size i, _, _]) = unfoldType callType

                        let (IR.ArgID argId) = (args !! 0)
                        let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId

                        cexpr <- generateArg (IR.ArgID argId)
                        cType <- cTypeOf argType
                        cRefType <- cRefTypeOf argType
                        base <- baseTypeOf argType

                        case unfoldType base of
                            (Sum, ts) -> do
                                let IR.Ref = ssaRefTyp
                                let ptr = C.Address (C.PMember cexpr $ "u" ++ show i)
                                base <- baseTypeOf (ts !! i)
                                cRefType <- cRefTypeOf (ts !! i)
                                case unfoldType base of
                                    (Type.Tuple, ts) -> do
                                        void $ appendAssign cRefType (idName id) $ C.Initialiser [ptr, C.Int 0, C.Int 1]
                                    (_, ts) -> void $ appendAssign cRefType (idName id) ptr

                            (Table, [t]) -> do
                                let IR.Ref = ssaRefTyp
                                baseT <- baseTypeOf t

                                case unfoldType baseT of
                                    (Tuple, ts) -> do
                                        ct <- cTypeOf baseT
                                        let off = C.Offsetof ct ("m" ++ show i)
                                        let ptr = C.Cast (Cpointer Cvoid) (C.PMember cexpr "r0")
                                        let row = C.Infix C.Plus ptr $ C.Infix C.Times off $ C.PMember cexpr "cap"
                                        -- setting slice cap to 0 represents non-flat memory
                                        slice <- assign "slice" $ Value (Apply Slice $ ts !! i) $
                                            C.Initialiser [ row, C.PMember cexpr "len", C.Int 0 ]

                                        Ref _ refExpr <- makeRef slice
                                        cRefType <- cRefTypeOf (Apply Slice $ ts !! i)
                                        void $ appendAssign cRefType (idName id) refExpr

                            (Tuple, ts) -> do
                                let IR.Ref = ssaRefTyp
                                cts <- mapM cTypeOf ts
                                ct <- cTypeOf base

                                let off = C.Offsetof ct ("m" ++ show i)
                                let ptr = C.Cast (Cpointer Cvoid) (C.Member cexpr "ptr")
                                let idx = C.Member cexpr "idx"
                                let cap = C.Member cexpr "cap"

                                case ssaRefTyp of
                                    IR.Ref -> do
                                        let ptr' = C.Cast (Cpointer $ cts !! i) $ C.Infix C.Plus ptr $ C.Infix Plus
                                                    (C.Infix C.Times cap off)
                                                    (C.Infix C.Times idx $ C.SizeofType $ cts !! i)

                                        baseField <- baseTypeOf (ts !! i)
                                        cRefTypeField <- cRefTypeOf (ts !! i)
                                        case unfoldType baseField of
                                            (Tuple, _) -> do
                                                void $ appendAssign cRefTypeField (idName id) $ C.Initialiser [ptr', C.Int 0, C.Int 1]
                                            (_, _) -> do
                                                void $ appendAssign cRefTypeField (idName id) ptr'

                                    x -> error (show x)
                            
                            x -> error (show x)


                    x -> do
                        header <- generateFunc callType
                        cArgs <- mapM generateArg args
                        let cCall = C.Call (showSymGlobal $ IR.irFuncSymbol header) cArgs

                        case (IR.irTypes funcIr) Map.! id of
                            (Void, IR.Const) -> void $ appendElem (C.ExprStmt cCall)
                            (typ, IR.Value) -> do
                                cType <- cTypeOf typ
                                void $ appendAssign cType (idName id) cCall

                            (typ, IR.Ref) -> do
                                cRefType <- cRefTypeOf typ
                                void $ appendAssign cRefType (idName id) cCall


                            x -> error (show x)


            IR.MakeValueFromReference (IR.ArgID argId) -> case (IR.irTypes funcIr) Map.! argId of
                (typ, IR.Ref) -> do
                    cType <- cTypeOf typ
                    cexpr <- generateArg (IR.ArgID argId)
                    Value _ cRef <- deref (Ref typ cexpr)
                    void $ appendElem $ C.Assign cType (idName id) cRef

                x -> error (show x)

    x -> error (show x)
