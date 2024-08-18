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

                    void $ appendElem $ C.ExprStmt $ C.Call
                        (showSymGlobal $ funcSymbol header)
                        (map valExpr args)

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

        --liftIO $ IR.prettyIR "" func

        symbol <- CGenerate.genSymbol (funcSymbol $ IR.irHeader func)
        let header' = (IR.irHeader func) { funcSymbol = symbol }


        modify $ \s -> s { astResolved = (astResolved s)
            { funcInstance = Map.insert funcType header' (funcInstance $ astResolved s) } }

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
        withCurID funcId (generateStmt func 0)

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
    IR.ConstFloat f  -> return (C.Float f)
    x -> error (show x)


generateStmt :: IR.FuncIR -> IR.ID -> Generate ()
generateStmt funcIr id = case (IR.irStmts funcIr) Map.! id of
    IR.Break -> void $ appendElem $ C.Break
    IR.Block ids -> mapM_ (generateStmt funcIr) ids
    IR.EmbedC _ str  -> void $ appendElem . C.Embed =<< processCEmbed str
    IR.Return arg -> void $ appendElem . C.Return =<< generateArg arg
    IR.ReturnVoid -> void $ appendElem C.ReturnVoid

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

        case funcSymbol of
            x | symbolsCouldMatch x (Sym ["builtin", "builtinTableAppend"]) -> do
                unless (length args == 1) (error "arg length mismatch")
                let (IR.ArgID argId) = head args
                let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId
                cexpr <- generateArg (head args)
                builtinTableAppend argType cexpr

            x | symbolsCouldMatch x (Sym ["builtin", "builtinTableLen"]) -> do
                unless (length args == 1) (error "arg length mismatch")
                let (IR.ArgID argId) = head args
                let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId
                cexpr <- generateArg (args !! 0)
                void $ appendAssign C.Cint64_t (idName id) (C.PMember cexpr "len")

            x | symbolsCouldMatch x (Sym ["builtin", "builtinArrayLen"]) -> do
                unless (length args == 1) (error "arg length mismatch")
                let (IR.ArgID argId) = (args !! 0)
                let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId

                let (Type.Array, [Size n, t]) = unfoldType argType
                void $ appendAssign C.Cint64_t (idName id) (C.Int $ fromIntegral n)

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

            x | symbolsCouldMatch x (Sym ["builtin", "builtinArrayAt"]) -> do
                unless (length args == 2) (error "arg length mismatch")

                carg <- generateArg (args !! 0)
                cidx <- generateArg (args !! 1)

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



            x -> do
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
                    (Table, [t]) -> do
                        baseT <- baseTypeOf t
                        case unfoldType baseT of
                            (Tuple, ts) -> do
                                ct <- cTypeOf baseT
                                let off = C.Offsetof ct ("m" ++ show i)
                                let ptr = C.Cast (Cpointer Cvoid) (C.Member cexpr "r0")
                                let row = C.Infix C.Plus ptr $ C.Infix C.Times off $ C.Member cexpr "cap"
                                -- setting slice cap to 0 represents non-flat memory
                                slice <- assign "slice" $ Value (Apply Slice $ ts !! i) $
                                    C.Initialiser [ row, C.Member cexpr "len", C.Int 0 ]

                                cType <- cTypeOf (Apply Slice $ ts !! i)
                                void $ appendAssign cType (idName id) (valExpr slice)

                            x -> error (show x)
            x -> error (show x)


    IR.MakeFieldFromRef argId i -> do
        let (argType, IR.Ref) = (IR.irTypes funcIr) Map.! argId
        cexpr <- generateArg (IR.ArgID argId)
        cType <- cTypeOf argType
        cRefType <- cRefTypeOf argType
        base <- baseTypeOf argType

        case unfoldType base of
            (Sum, ts) -> do
                let (_, IR.Ref) = (IR.irTypes funcIr) Map.! id

                let ptr = C.Address (C.PMember cexpr $ "u" ++ show i)
                base <- baseTypeOf (ts !! i)
                cRefType <- cRefTypeOf (ts !! i)
                case unfoldType base of
                    (Type.Tuple, ts) -> do
                        void $ appendAssign cRefType (idName id) $ C.Initialiser [ptr, C.Int 0, C.Int 1]
                    (_, ts) -> void $ appendAssign cRefType (idName id) ptr

            (Table, [t]) -> do
                let (_, IR.Ref) = (IR.irTypes funcIr) Map.! id
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
                cts <- mapM cTypeOf ts
                ct <- cTypeOf base

                let off = C.Offsetof ct ("m" ++ show i)
                let ptr = C.Cast (Cpointer Cvoid) (C.Member cexpr "ptr")
                let idx = C.Member cexpr "idx"
                let cap = C.Member cexpr "cap"

                case (IR.irTypes funcIr) Map.! id of
                    (_, IR.Value) -> do
                        cTypeField <- cTypeOf (ts !! i)
                        void $ appendAssign cTypeField (idName id) $ C.Deref $
                            C.Cast (Cpointer cTypeField) $ C.Infix C.Plus ptr $ C.Infix Plus
                                (C.Infix C.Times cap off)
                                (C.Infix C.Times idx $ C.SizeofType cTypeField)

                    (_, IR.Ref) -> do
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

    x -> error (show x)
