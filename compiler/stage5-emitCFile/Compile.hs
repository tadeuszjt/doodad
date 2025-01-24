module Compile where

import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Reader

import CGenerate
import CAst as C
import CBuilder as C
import Type
import ASTResolved
import Symbol
import Error
import Builtin

import Ir2


generateAst :: ASTResolved -> Either Error (((), GenerateState), BuilderState)
generateAst ast = runGenerate ast
    (initGenerateState $ ASTResolved.moduleName ast) C.initBuilderState generate


generate :: Generate ()
generate = withErrorPrefix "generate: " $ do
    ast <- ask

    -- generate all function headers
    forM_ (Map.toList $ instantiations ast) $ \(funcType, funcIr) -> do
        let (TypeDef funcSymbol, _) = unfoldType funcType
        case funcSymbol of
            SymResolved ("builtin" : ('b':'u':'i':'l':'t':'i':'n':_) : _) -> return ()
            _ -> do
                (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf funcType
                unless (length argTypes == length (irArgs funcIr)) (error "arg mismatch")

                cReturnType <- case irReturn funcIr of
                    ParamValue _ -> cTypeOf retType
                    ParamModify _ -> cRefTypeOf retType
                    x -> error (show x)

                cArgTypes <- forM (zip argTypes $ irArgs funcIr) $ \(argType, arg) -> case arg of
                    ParamModify _ -> cRefTypeOf argType
                    ParamValue _ -> cTypeOf argType

                cCtxTypes <- forM (Set.toList $ fromJust $ irContexts funcIr) $ \typ -> cRefTypeOf typ
                appendExtern
                    (showSymGlobal $ irSymbol funcIr)
                    cReturnType
                    (cArgTypes ++ cCtxTypes)
                    [C.Extern]


    -- generate top function definitions
    forM_ (instantiationsTop ast) $ \instType -> do
        let Just funcIr = Map.lookup instType (instantiations ast)
        let (TypeDef funcSymbol, _) = unfoldType instType
        case funcSymbol of
            SymResolved ("builtin" : ('b':'u':'i':'l':'t':'i':'n':_) : _) -> return ()

            s | symbolsCouldMatch s (Sym [ASTResolved.moduleName ast, "main"]) -> do
                generateFuncIr funcIr
                mainId <- newFunction C.Cint "main"
                    [C.Param "argc" C.Cint, C.Param "argv" (C.Cpointer $ C.Cpointer C.Cchar)]
                    []
                withCurID mainId $ do
                    (Type.Func, mainRetType : mainArgTypes) <- unfoldType <$> baseTypeOf instType
                    cts <- mapM cTypeOf mainArgTypes
                    args <- forM mainArgTypes $ \argType -> do
                        cType <- cTypeOf argType
                        name <- fresh "mainArg"
                        base <- baseTypeOf argType
                        appendAssign cType name $ C.Initialiser [C.Int 0]
                        return (C.Ident name)

                    appendElem $ C.ExprStmt $ C.Call (showSymGlobal $ irSymbol funcIr) args
                    appendElem $ C.Return (C.Int 0)

                withCurID globalID (append mainId)


            _ -> generateFuncIr funcIr


generateFuncIr :: FuncIr2 -> Generate ()
generateFuncIr funcIr = do
    cArgs <- forM (zip (irArgs funcIr) [1..]) $ \(arg, i) -> case arg of
        ParamModify typ -> C.Param (idName i) <$> cRefTypeOf typ
        ParamValue  typ -> C.Param (idName i) <$> cTypeOf typ

    cCtxs <- forM (zip [0..] $ Set.toList $ fromJust $ irContexts funcIr) $ \(i, typ) -> do
        C.Param ("_ctx" ++ show i) <$> cRefTypeOf typ

    cReturn <- case irReturn funcIr of
        ParamValue typ -> cTypeOf typ
        ParamModify typ -> cRefTypeOf typ

    funcId <- newFunction cReturn (showSymGlobal $ irSymbol funcIr) (cArgs ++ cCtxs) []
    withCurID funcId (generateStmt funcIr 0)
    withCurID globalID (append funcId)
    return ()


idName :: Ir2.ID -> String
idName x = "_" ++ show x


processCEmbed :: [(String, Ir2.ID)] -> String -> Generate String
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


generateArg :: FuncIr2 -> Arg -> Generate C.Expression
generateArg funcIr arg = do
    base <- baseTypeOf arg
    case unfoldType base of
        (Slice, [t]) -> case arg of
            ArgValue _ id -> return $ C.Ident (idName id)
            ArgModify _ id -> return $ C.Ident (idName id)
        _ -> case arg of
            ArgValue typ id -> case Map.lookup id (irIdArgs funcIr) of
                Just (ArgValue _ _) -> return $ C.Ident (idName id)
                Just (ArgModify _ _) -> deref typ $ C.Ident (idName id)
                Nothing -> error (show id)

            ArgModify typ id -> case Map.lookup id (irIdArgs funcIr) of
                Just (ArgModify _ _) -> return $ C.Ident (idName id)
                Just (ArgValue _ _ ) -> stackRef typ $ C.Ident (idName id)

            x -> error (show x)


generateStmt :: FuncIr2 -> Ir2.ID -> Generate ()
generateStmt funcIr id = case (irStmts funcIr) Map.! id of
    Block ids          -> mapM_ (generateStmt funcIr) ids
    Ir2.Return Nothing -> do
        -- TODO return void
        cType <- cTypeOf Tuple
        name <- fresh "empty"
        appendElem $ C.Assign cType name (C.Initialiser [])
        void $ appendElem (C.Return $ C.Ident name)

    Ir2.Call typ args  -> generateCall funcIr typ id args

    Ir2.Return (Just id) -> case irReturn funcIr of
        ParamValue typ -> void $ appendElem . C.Return =<< generateArg funcIr (ArgValue typ id)
        ParamModify typ -> void $ appendElem . C.Return =<< generateArg funcIr (ArgModify typ id)

    Ir2.If id ids -> do
        val <- generateArg funcIr (ArgValue Type.Bool id)
        ifId <- appendElem (C.If val [])
        withCurID ifId $ mapM_ (generateStmt funcIr) ids

    Ir2.Loop ids -> do
        forId <- appendElem $ C.For Nothing Nothing Nothing []
        withCurID forId $ mapM_ (generateStmt funcIr) ids

    Ir2.Break -> void (appendElem C.Break)

    Ir2.EmbedC strMap str-> void $ appendElem . C.Embed =<< processCEmbed strMap str

    Ir2.MakeSlice typ args -> do
        cs <- mapM (generateArg funcIr) args

        base <- baseTypeOf typ
        case unfoldType base of
            (Slice, [t]) -> do
                cType <- C.Carray (length args) <$> cTypeOf t
                name <- fresh "slice"
                appendAssign cType name (C.Initialiser cs)
                cSlice <- cTypeOf typ
                void $ appendAssign cSlice (idName id) $ C.Initialiser
                    [ C.Ident name                       --  ptr
                    , C.Int 0                            -- cap
                    , C.Int 0                            -- start
                    , C.Int (fromIntegral $ length args) -- end
                    ]

            x -> error (show x)

    x -> error (show x)


generateCall :: FuncIr2 -> Type.Type -> Ir2.ID -> [Ir2.Arg] -> Generate ()
generateCall funcIr funcType id args = do
    let (TypeDef funcSymbol, _) = unfoldType funcType
    case funcSymbol of
        s | symbolsCouldMatch s (Sym ["builtin", "builtinAdd"]) -> do
            [ca, cb] <- mapM (generateArg funcIr) args
            [ta, tb] <- mapM cTypeOf args
            unless (ta == tb) (error "")
            void $ appendAssign ta (idName id) (C.Infix C.Plus ca cb)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinSubtract"]) -> do
            [ca, cb] <- mapM (generateArg funcIr) args
            [ta, tb] <- mapM cTypeOf args
            unless (ta == tb) (error "")
            void $ appendAssign ta (idName id) (C.Infix C.Minus ca cb)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinMultiply"]) -> do
            [ca, cb] <- mapM (generateArg funcIr) args
            [ta, tb] <- mapM cTypeOf args
            unless (ta == tb) (error "")
            void $ appendAssign ta (idName id) (C.Infix C.Times ca cb)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinDivide"]) -> do
            [ca, cb] <- mapM (generateArg funcIr) args
            [ta, tb] <- mapM cTypeOf args
            unless (ta == tb) (error "")
            void $ appendAssign ta (idName id) (C.Infix C.Divide ca cb)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinModulo"]) -> do
            [ca, cb] <- mapM (generateArg funcIr) args
            [ta, tb] <- mapM cTypeOf args
            unless (ta == tb) (error "")
            void $ appendAssign ta (idName id) (C.Infix C.Modulo ca cb)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinEqual"]) -> do
            [ca, cb] <- mapM (generateArg funcIr) args
            [ta, tb] <- mapM cTypeOf args
            unless (ta == tb) (error "")
            void $ appendAssign C.Cbool (idName id) (C.Infix C.EqEq ca cb)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinLessThan"]) -> do
            [ca, cb] <- mapM (generateArg funcIr) args
            [ta, tb] <- mapM cTypeOf args
            unless (ta == tb) (error "")
            void $ appendAssign C.Cbool (idName id) (C.Infix C.LT ca cb)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinNot"]) -> do
            [cexpr] <- mapM (generateArg funcIr) args
            void $ appendAssign C.Cbool (idName id) (C.Infix C.EqEq cexpr $ C.Bool False)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinConvert"]) -> do
            [cexpr] <- mapM (generateArg funcIr) args
            [cType] <- mapM cTypeOf args
            void $ appendAssign cType (idName id) cexpr

        s | symbolsCouldMatch s (Sym ["builtin", "builtinArrayLen"]) -> do
            [(Type.Array, [Size n, t])] <- map unfoldType <$> mapM baseTypeOf args
            void $ appendAssign C.Cint64_t (idName id) (C.Int $ fromIntegral n)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinContext"]) -> do
            let (TypeDef symbol, [t]) = unfoldType funcType
            let Just i = elemIndex t (Set.toList $ fromJust $ irContexts funcIr)
            cRefType <- cRefTypeOf t
            void $ appendAssign cRefType (idName id) (C.Ident $ "_ctx" ++ show i)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinSlice"]) -> do
            [cexpr, cstart, cend] <- mapM (generateArg funcIr) args

            base <- baseTypeOf (args !! 0)
            case unfoldType base of
                (Table, [t]) -> do
                    cType <- cTypeOf (Apply Slice t)
                    void $ appendAssign cType (idName id) $ C.Initialiser
                        [ C.PMember cexpr "r0" , C.PMember cexpr "cap" , cstart , cend ]

                (Array, [Size n, t]) -> do
                    cType <- cTypeOf (Apply Slice t)
                    void $ appendAssign cType (idName id) $ C.Initialiser
                        [ C.PMember cexpr "arr" , C.Int 0 , cstart , cend ]

                (Slice, [t]) -> do
                    cType <- cTypeOf (Apply Slice t)
                    void $ appendAssign cType (idName id) $ C.Initialiser
                        [ C.Member cexpr "ptr", C.Member cexpr "cap", cstart, cend ]

        s | symbolsCouldMatch s (Sym ["builtin", "builtinSliceLen"]) -> do
            [cexpr] <- mapM (generateArg funcIr) args
            void $ appendAssign C.Cint64_t (idName id) $ C.Infix C.Minus
                (C.Member cexpr "end")
                (C.Member cexpr "start")

        s | symbolsCouldMatch s (Sym ["builtin", "builtinPretend"]) -> do
            [cexpr] <- mapM (generateArg funcIr) args
            cRefType <- cRefTypeOf (args !! 0)
            void $ appendAssign cRefType (idName id) cexpr

        s | symbolsCouldMatch s (Sym ["builtin", "builtinArrayAt"]) -> do
            [carg, cidx] <- mapM (generateArg funcIr) args
            let cref = C.Address $ C.Subscript (C.PMember carg "arr") cidx

            (Array, [Size n, t]) <- unfoldType <$> baseTypeOf (args !! 0)
            base <- baseTypeOf t
            cRefType <- cRefTypeOf t
            case unfoldType base of
                (Tuple, (_:_)) -> void $ appendAssign cRefType (idName id) $ C.Initialiser [cref, C.Int 0, C.Int 1]
                _              -> void $ appendAssign cRefType (idName id) cref

        s | symbolsCouldMatch s (Sym ["builtin", "builtinSliceAt"]) -> do
            [cexpr, cidx] <- mapM (generateArg funcIr) args
            (Slice, [t]) <- unfoldType <$> baseTypeOf (args !! 0)
            base <- baseTypeOf t
            cRefType <- cRefTypeOf t

            -- ptr = ptr + (cap ? 0 : sizeof(struct) * (idx + start)
            -- idx = cap ? (idx + start) : 0
            -- cap = cap ? cap : 1
            let start = C.Member cexpr "start"
            case unfoldType base of
                (Tuple, ts) -> do 
                    ct <- cTypeOf base
                    let ptr = C.Infix C.Plus
                            (C.Cast (C.Cpointer C.Cvoid) $ C.Member cexpr "ptr")
                            (C.CndExpr (C.Member cexpr "cap")
                                (C.Int 0)
                                (C.Infix C.Times (C.SizeofType ct) (C.Infix C.Plus cidx start)))

                    let idx = C.CndExpr (C.Member cexpr "cap") (C.Infix C.Plus cidx start) (C.Int 0)
                    let cap = C.CndExpr (C.Member cexpr "cap") (C.Member cexpr "cap") (C.Int 1)

                    void $ appendAssign cRefType (idName id) $ C.Initialiser [ptr, idx, cap]

                (_, _) -> void $ appendAssign cRefType (idName id) $ C.Address $
                    C.Subscript (C.Member cexpr "ptr") (C.Infix C.Plus start cidx)

        s | symbolsCouldMatch s (Sym ["builtin", "builtinStore"]) -> do
            let (TypeDef _, [typ]) = unfoldType funcType
            [cexpr1, cexpr2] <- mapM (generateArg funcIr) args
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

        s | symbolsCouldMatch s (Sym ["builtin", "builtinSumReset"]) -> do
            [ca, cb] <- mapM (generateArg funcIr) args
            (Sum, _) <- unfoldType <$> baseTypeOf (args !! 0)
            appendElem $ C.ExprStmt $ C.Call "memset" [ca, C.Int 0, C.Sizeof (C.Deref ca)]
            appendElem $ C.Set (C.PMember ca "en") cb
            return ()

        x | symbolsCouldMatch x (Sym ["builtin", "builtinSumEnum"]) -> do
            [cexpr] <- mapM (generateArg funcIr) args
            void $ appendAssign C.Cint64_t (idName id) (C.PMember cexpr "en")

        s | symbolsCouldMatch s (Sym ["builtin", "builtinTableAppend"]) -> do
            [cexpr] <- mapM (generateArg funcIr) args
            builtinTableAppend (typeof $ args !! 0) cexpr

        s | symbolsCouldMatch s (Sym ["builtin", "builtinTableLen"]) -> do
            [cexpr] <- mapM (generateArg funcIr) args
            void $ appendAssign C.Cint64_t (idName id) (C.PMember cexpr "len")

        s | symbolsCouldMatch s (Sym ["builtin", "builtinTableAt"]) -> do
            [cexpr, cidx] <- mapM (generateArg funcIr) args
            Apply Table t <- baseTypeOf (args !! 0)
            base <- baseTypeOf t
            cRefType <- cRefTypeOf t
            case unfoldType base of
                (Tuple, ts) -> void $ appendAssign cRefType (idName id) $
                    C.Initialiser [C.PMember cexpr "r0", cidx, C.PMember cexpr "cap"]
                _ -> void $ appendAssign cRefType (idName id) $
                    C.Address $ C.Subscript (C.PMember cexpr "r0") cidx

        s | symbolsCouldMatch s (Sym ["builtin", "builtinInit"]) -> do
            let [ArgConst typ const] = args
            cType <- cTypeOf typ
            base <- baseTypeOf typ
            cexpr <- case (unfoldType base, const) of
                ((I64, []), ConstInt n)               -> return (C.Int n)
                ((F64, []), ConstFloat f)             -> return (C.Float f)
                ((Type.Bool, []), ConstBool b)        -> return (C.Bool b)
                ((Type.Char, []), ConstChar c)        -> return (C.Char c)
                ((Tuple, []), ConstZero)              -> return $ C.Initialiser []
                ((Sum, _), ConstZero)                 -> return $ C.Initialiser [C.Int 0]
                ((Tuple, _), ConstZero)               -> return $ C.Initialiser [C.Int 0]
                ((_, _), ConstZero)                   -> return $ C.Initialiser [C.Int 0]
                ((Slice, [Type.Char]), ConstString s) -> do
                    return $ C.Initialiser
                        [ C.String s
                        , C.Int 0
                        , C.Int 0
                        , C.Int (fromIntegral $ length s)
                        ]
                x -> error (show x)

            void $ appendAssign cType (idName id) cexpr

        s | symbolsCouldMatch s (Sym ["builtin", "builtinField"]) -> do
            [cexpr] <- mapM (generateArg funcIr) args
            let (TypeDef _, [Size i, _, _]) = unfoldType funcType

            cType <- cTypeOf (args !! 0)
            cRefType <- cRefTypeOf (args !! 0)
            base <- baseTypeOf (args !! 0)

            case unfoldType base of
                (Sum, ts) -> do
                    let ptr = C.Address (C.PMember cexpr $ "u" ++ show i)
                    base <- baseTypeOf (ts !! i)
                    cRefType <- cRefTypeOf (ts !! i)
                    case unfoldType base of
                        (Type.Tuple, ts) -> do
                            void $ appendAssign cRefType (idName id) $ C.Initialiser [ptr, C.Int 0, C.Int 1]
                        (_, ts) -> void $ appendAssign cRefType (idName id) ptr

                (Table, [t]) -> do
                    baseT <- baseTypeOf t

                    case unfoldType baseT of
                        (Tuple, ts) -> do
                            ct <- cTypeOf baseT
                            let off = C.Offsetof ct ("m" ++ show i)
                            let ptr = C.Cast (Cpointer Cvoid) (C.PMember cexpr "r0")
                            let row = C.Infix C.Plus ptr $ C.Infix C.Times off $ C.PMember cexpr "cap"
                            cSlice <- cTypeOf (Apply Slice $ ts !! i)

                            -- setting slice cap to 0 represents non-flat memory
                            void $ appendElem $ C.Assign cSlice (idName id) $ C.Initialiser
                                [ row
                                , C.Int 0
                                , C.Int 0
                                , C.PMember cexpr "len"
                                ]

                (Tuple, ts) -> do
                    cts <- mapM cTypeOf ts
                    ct <- cTypeOf base

                    let off = C.Offsetof ct ("m" ++ show i)
                    let ptr = C.Cast (Cpointer Cvoid) (C.Member cexpr "ptr")
                    let idx = C.Member cexpr "idx"
                    let cap = C.Member cexpr "cap"

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

        s -> do
            Just callIr <- Map.lookup funcType . instantiations <$> ask
            cRetTy <- case irReturn callIr of
                ParamModify typ -> cRefTypeOf typ
                ParamValue typ  -> cTypeOf typ

            void $ appendAssign cRetTy (idName id) =<<
                C.Call (showSymGlobal $ irSymbol callIr) <$> mapM (generateArg funcIr) args

        x -> error (show x)
        
