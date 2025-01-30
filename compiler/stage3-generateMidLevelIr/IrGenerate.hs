{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IrGenerate where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except

import Error
import Type
import Ir
import ASTResolved
import Symbol
import AstBuilder
import FindFunc
import qualified AST
import qualified InstBuilder as Inst


data IrGenerateState = IrGenerateState
    { symbolTable    :: Map.Map Symbol ID
    , copyReturnFlag :: Bool -- turn this off when generating builtin::copy
    }


initIrGenerateState = IrGenerateState
    { symbolTable    = Map.empty
    , copyReturnFlag = True
    }



newtype IrGenerate a = IrGenerate
    { unIrGenerate :: StateT IrGenerateState (StateT FuncIr (StateT ASTResolved (Except Error))) a }
    deriving (Functor, Applicative, Monad, MonadState IrGenerateState, MonadError Error)


liftAstState :: StateT ASTResolved (Except Error) a -> IrGenerate a
liftAstState = IrGenerate . lift . lift


instance MonadFail IrGenerate where
    fail s = throwError (ErrorStr s)


runIrGenerateAst :: FuncIr -> IrGenerateState -> IrGenerate a
    -> StateT ASTResolved (Except Error) (a, FuncIr)
runIrGenerateAst funcIr irGenerateState f = do
    ((a, _), funcIr) <- runStateT (runStateT (unIrGenerate f) irGenerateState) funcIr
    return (a, funcIr)


instance MonadFuncIr (IrGenerate) where
    liftFuncIrState (StateT s) = IrGenerate $ lift $ state (runIdentity . s)


instance MonadTypeDefs (IrGenerate) where
    getTypeDefs = liftAstState (gets typeDefsAll)


-- generate IR for all instances with no generics
irGenerateAst :: StateT ASTResolved (Except Error) ()
irGenerateAst = do
    ast <- get
    forM_ (instancesTop ast) $ \instSymbol -> case Map.lookup instSymbol (instancesAll ast) of
        Just (TopInst _ [] callType _ _ _) -> void $ irGenerateInstance callType
        Just (TopField _ [] callType _)    -> void $ irGenerateInstance callType
        Nothing                            -> error "no instance"
        _ -> return ()


-- ensure generated IR for function, return underlying call type
irGenerateInstance :: Type -> StateT ASTResolved (Except Error) Type
irGenerateInstance callType = do
    ast <- get
    case isNothing (Map.lookup callType $ instantiations ast) of
        False -> return callType
        True -> do
            topStmt <- case runExcept (makeInstantiation ast callType) of
                Right (Just topStmt) -> return topStmt
                Left e               -> throwError e

            instType <- case topStmt of
                TopInst _ [] typ _ _ _ -> return typ
                TopField _ [] typ _    -> return typ

            callType' <- case instType == callType of
                False -> irGenerateInstance instType
                True -> case topStmt of
                    TopInst _ _ _ _ _ _ -> do
                        runIrGenerateAst initFuncIr initIrGenerateState (irGenerateTopInst topStmt)
                        return callType
                    TopField _ _ _ _    -> do
                        runIrGenerateAst initFuncIr initIrGenerateState (irGenerateTopField topStmt)
                        return callType

            irGenerateDestroyInst callType'
            return callType'


-- adds a call, ensures generated IR
irGenerateCall :: Type -> [ID] -> IrGenerate ID
irGenerateCall callType argIds = do
    callType' <- liftAstState $ irGenerateInstance callType
    (Func, retType : argTypes) <- unfoldType <$> baseTypeOf callType'
    Just funcIr <- liftAstState $ gets (Map.lookup callType' . instantiations)

    unless (length argIds == length (irArgs funcIr)) $ do
        error ("arg length mismatch for: " ++ show callType')
    
    args <- forM (zip3 argIds argTypes $ irArgs funcIr) $ \(argId, argType, irArg) -> case irArg of
        ParamValue typ -> do
            unless (typ == argType) (error "arg type mismatch")
            return (ArgValue typ argId)
        ParamModify typ -> do
            unless (typ == argType) (error "arg type mismatch")
            return (ArgModify typ argId)

    id <- appendStmt $ Call callType' args

    case irReturn funcIr of
        ParamValue typ -> addIdArg id (ArgValue typ id)
        ParamModify typ -> addIdArg id (ArgModify typ id)

    return id


define :: Symbol -> ID -> IrGenerate ()
define symbol id = do
    resm <- gets (Map.lookup symbol . symbolTable)
    when (isJust resm) (error "symbol already defined")
    modify $ \s -> s { symbolTable = Map.insert symbol id (symbolTable s) }


look :: Symbol -> IrGenerate ID
look symbol = do
    Just id <- gets (Map.lookup symbol . symbolTable)
    return id


irGenerateTopField :: TopStmt -> IrGenerate ()
irGenerateTopField (TopField _ [] funcType i) = do
    let (TypeDef funcSymbol, _) = unfoldType funcType
    (Func, retType : argTypes) <- unfoldType <$> baseTypeOf funcType
    (Sum, ts)                  <- unfoldType <$> baseTypeOf retType

    symbol <- liftAstState (genSymbol $ SymResolved [typeCode funcType])
    liftFuncIrState $ modify $ \s -> s
        { irReturn = ParamValue retType
        , irSymbol = symbol
        }

    argIds <- forM argTypes $ \argType -> do
        id <- generateId
        addIdArg id (ArgValue argType id)
        liftFuncIrState $ modify $ \s -> s { irArgs = (irArgs s) ++ [ParamValue argType] }
        return id

    sum <- initVar retType ConstZero
    n   <- initVar I64 $ ConstInt (fromIntegral i)
    call ["builtin", "builtinSumReset"] [retType] [sum, n]
    field <- call ["builtin", "builtinField"] [Size i, ts !! i, retType] [sum]

    case argTypes of
        [] -> return ()
        [argType] -> void $ call ["builtin", "store"] [argType] [field, head argIds]
        xs -> do
            let tupType = ts !! i
            forM_ (zip xs [0..]) $ \(x, j) -> do
                ref <- call ["builtin", "builtinField"] [Size j, x, tupType] [field]
                call ["builtin", "store"] [x] [ref, argIds !! j]

    appendStmt (Return $ Just sum)

    funcIr <- liftFuncIrState get
    liftAstState $ modify $ \s -> s
        { instantiations = Map.insert funcType funcIr (instantiations s)
        , instantiationsTop = Set.insert funcType (instantiationsTop s)
        }

            
irGenerateTopInst :: TopStmt -> IrGenerate ()
irGenerateTopInst (TopInst _ [] callType args isRef inst) = do
    --liftIO $ putStrLn $ "irGenerateTopInst: " ++ show callType
    let (TypeDef funcSymbol, _) = unfoldType callType
    when (symbolsCouldMatch (Sym ["builtin", "copy"]) funcSymbol) $ modify $ \s -> s
        { copyReturnFlag = False }

    (Func, retType : argTypes) <- unfoldType <$> baseTypeOf callType
    unless (length argTypes == length args) (error "args mismatch")
    case isRef of
        True -> liftFuncIrState $ modify $ \s -> s { irReturn = ParamModify retType }
        False -> liftFuncIrState $ modify $ \s -> s { irReturn = ParamValue retType }

    symbol <- liftAstState (genSymbol $ SymResolved [typeCode callType])
    liftFuncIrState $ modify $ \s -> s { irSymbol = symbol }

    forM_ args $ \arg -> case arg of
        AST.Param _ symbol typ -> do 
            id <- generateId
            define symbol id
            addIdArg id (ArgValue typ id)
            liftFuncIrState $ modify $ \s -> s { irArgs = (irArgs s) ++ [ParamValue typ] }

        AST.RefParam _ symbol typ -> do
            id <- generateId
            define symbol id
            addIdArg id (ArgModify typ id)
            liftFuncIrState $ modify $ \s -> s { irArgs = (irArgs s) ++ [ParamModify typ] }

        x -> error (show x)


    -- predefine header
    funcIrHeader <- liftFuncIrState get
    liftAstState $ modify $
        \s -> s { instantiations = Map.insert callType funcIrHeader (instantiations s) }

    irGenerateStmt inst (AST.Stmt 0)

    funcIr <- liftFuncIrState get
    liftAstState $ modify $ \s -> s
        { instantiations = Map.insert callType funcIr (instantiations s)
        , instantiationsTop = Set.insert callType (instantiationsTop s)
        }
            
            
irGenerateStmt :: Inst.InstBuilderState -> AST.Stmt -> IrGenerate ()
irGenerateStmt inst (AST.Stmt id) = case Inst.statements inst Map.! id of
    AST.ExprStmt expr -> void $ irGenerateExpr inst expr
    AST.Block ids     -> mapM_ (irGenerateStmt inst) ids
    AST.Return _ Nothing -> void $ appendStmt (Return Nothing)

    AST.Return _ (Just expr) -> do
        id <- irGenerateExpr inst expr
        irRet <- liftFuncIrState (gets irReturn)
        stmtm <- liftFuncIrState $ gets (Map.lookup id . irStmts)
        flag <- gets copyReturnFlag

        void $ appendStmt . Return . Just =<< case irRet of
            ParamModify typ -> case stmtm of
                Just (Call _ _) -> return id
                Nothing         -> return id

            ParamValue typ -> case stmtm of -- copy if argument or ref
                Nothing         -> do
                    base <- baseTypeOf typ
                    case unfoldType base of
                        (Slice, _) -> return id
                        _          -> case flag of
                            True -> call ["builtin", "copy"] [Inst.typeOfExpr inst expr] [id]
                            False -> return id
                            
                Just (Call _ _) -> do
                    base <- baseTypeOf typ
                    case unfoldType base of
                        (Slice, _) -> return id
                        _ -> do
                            arg' <- getIdArg id
                            case arg' of
                                ArgValue _ _ -> return id
                                ArgModify _ _ -> call ["builtin", "copy"] [Inst.typeOfExpr inst expr] [id]
                                x -> error (show x)


    AST.EmbedC _ strMap str -> do
        strMap' <- forM strMap $ \(s, symbol) -> do
            id' <- look symbol
            return (s, id')
        void $ appendStmt (EmbedC strMap' str)

    AST.With _ exprs blk -> do
        args <- forM exprs $ \expr -> do
            id <- irGenerateExpr inst expr
            let typ = Inst.typeOfExpr inst expr 
            return $ ArgModify typ id

        withId <- appendStmt (With args [])
        withCurrentId withId $ irGenerateStmt inst blk


    AST.If _ cnd true mfalse -> do
        curId <- getCurrentId
        arg <- irGenerateCondition inst curId cnd 
        case mfalse of
            Nothing -> do
                ifId <- appendStmt $ If arg []
                withCurrentId ifId $ irGenerateStmt inst true
            Just false -> do
                loopId <- appendStmt $ Loop []
                withCurrentId loopId $ do
                    ifId <- appendStmt $ If arg []
                    withCurrentId ifId $ do
                        irGenerateStmt inst true
                        appendStmt Break
                    irGenerateStmt inst false
                    void $ appendStmt Break

    AST.Let _ (AST.Pattern patId) Nothing Nothing -> do
        val <- initVar (Inst.typeOfPat inst $ AST.Pattern patId) ConstZero
        case Inst.patterns inst Map.! patId of
            AST.PatIdent _ symbol -> do
                define symbol val

    AST.Let _ (AST.Pattern patId) (Just expr) Nothing -> do
        id <- irGenerateExpr inst expr
        case Inst.patterns inst Map.! patId of
            AST.PatIdent _ symbol -> do
                let typ = Inst.typeOfPat inst (AST.Pattern patId)
                stmtm <- getStmt id
                define symbol =<< case stmtm of
                    Just (Call _ _) -> do
                        callArg <- getIdArg id
                        case callArg of
                            ArgValue _ _ -> return id
                            _            -> call ["builtin", "copy"] [typ] [id]
                    Nothing              -> call ["builtin", "copy"] [typ] [id]

            _ -> do
                curId <- getCurrentId
                b <- irGeneratePattern inst curId (AST.Pattern patId) id
                void $ call ["assert", "assert"] [] [b]

    AST.With pos exprs blk -> do
        error "with"


    AST.Switch _ expr cases -> do
        arg <- irGenerateExpr inst expr
        loopId <- appendStmt (Loop [])
        withCurrentId loopId $ do
            forM_ cases $ \(pat, stmt) -> do
                b <- irGeneratePattern inst loopId pat arg
                ifId <- appendStmt (If b [])
                withCurrentId ifId $ do
                    irGenerateStmt inst stmt
                    appendStmt Break
            
            -- todo assert
            void $ appendStmt Break

    AST.While _ cnd stmt -> do
        loopId <- appendStmt (Loop [])

        withCurrentId loopId $ do
            b <- irGenerateCondition inst loopId cnd
            n <- call ["builtin", "builtinNot"] [Type.Bool] [b]
            ifId <- appendStmt (If n [])
            withCurrentId ifId (appendStmt Break)
            irGenerateStmt inst stmt

    AST.For _ expr mpat (AST.Stmt blkId) -> do
        idx <- initVar I64 (ConstInt 0)

        loopId <- appendStmt (Loop [])
        void $ withCurrentId loopId $ do
            arg <- irGenerateExpr inst expr

            end <- call ["container", "forEnd"] [Inst.typeOfExpr inst expr] [arg]
            lte <- call ["builtin", "builtinLessThan"] [Type.Bool] [idx, end]
            not <- call ["boolean", "not"] [Type.Bool] [lte]
            ifId <- appendStmt (If not [])
            withCurrentId ifId (appendStmt Break)

            case mpat of
                Nothing -> return ()
                Just pat -> do
                    elem <- call ["container", "forAt"]
                        [Inst.typeOfPat inst pat, Inst.typeOfExpr inst expr]
                        [arg, idx]
                    b <- irGeneratePattern inst loopId pat elem
                    not <- call ["boolean", "not"] [Type.Bool] [b]
                    
                    ifId <- appendStmt (If not []) 
                    void $ withCurrentId ifId (appendStmt Break)


            let Just (AST.Block stmts) = Map.lookup blkId (Inst.statements inst)
            mapM_ (irGenerateStmt inst) stmts

            n1 <- initVar I64 (ConstInt 1)
            ip1 <- call ["arithmetic", "add"] [I64] [idx, n1]
            call ["builtin", "store"] [I64] [idx, ip1]


    x -> error (show x)


irGeneratePattern :: Inst.InstBuilderState -> ID -> AST.Pattern -> ID -> IrGenerate ID
irGeneratePattern inst defBlkId (AST.Pattern patId) argId = case Inst.patterns inst Map.! patId of
    AST.PatIgnore _ -> withCurrentId defBlkId $ initVar Type.Bool (ConstBool True)
    AST.PatLiteral expr -> do
        val <- irGenerateExpr inst expr
        call ["compare", "equal"] [Inst.typeOfPat inst (AST.Pattern patId)] [val, argId]

    AST.PatField _ symbol pat -> do
        Just (_, i) <- liftAstState $ gets (Map.lookup symbol . fieldsAll)
        let sumType   = Inst.typeOfPat inst (AST.Pattern patId)
        let fieldType = Inst.typeOfPat inst pat

        en  <- call ["builtin", "builtinSumEnum"] [sumType] [argId]
        n   <- initVar I64 $ ConstInt (fromIntegral i)
        match <- call ["builtin", "builtinEqual"] [I64] [en, n]

        ifId <- appendStmt (If match [])
        withCurrentId ifId $ do
            field <- call ["builtin", "builtinField"] [Size i, fieldType, sumType] [argId]
            b <- irGeneratePattern inst defBlkId pat field
            call ["builtin", "store"] [Type.Bool] [match, b]

        return match


    AST.PatTuple _ pats -> do
        match <- withCurrentId defBlkId $ initVar Type.Bool (ConstBool True)

        loopId <- appendStmt (Loop [])
        withCurrentId loopId $ do
            forM_ (zip pats [0..]) $ \(pat@(AST.Pattern fieldId), i) -> do
                let Just patType   = Map.lookup patId (Inst.types inst)
                let Just fieldType = Map.lookup fieldId (Inst.types inst)

                field <- call
                    ["tuple", "tuplePattern"]
                    [fieldType, Size (length pats), Size i, patType]
                    [argId]


                b <- irGeneratePattern inst defBlkId pat field
                not <- call ["boolean", "not"] [Type.Bool] [b]

                ifId <- appendStmt (If not [])
                withCurrentId ifId $ do
                    false <- initVar Bool (ConstBool False)
                    call ["builtin", "store"] [Type.Bool] [match, false]
                    appendStmt Break

            appendStmt Break

        return match

    AST.PatGuarded _ pat expr -> do
        patMatch <- irGeneratePattern inst defBlkId pat argId
        match <- withCurrentId defBlkId $ initVar Type.Bool (ConstBool False)

        ifId <- appendStmt (If patMatch [])
        withCurrentId ifId $ do
            b <- irGenerateCondition inst defBlkId expr
            call ["builtin", "store"] [Type.Bool] [match, b]

        return match

    AST.PatIdent _ symbol -> do
        let typ = Inst.typeOfPat inst (AST.Pattern patId)
        value <- withCurrentId defBlkId $ initVar typ ConstZero
        call ["builtin", "store"] [typ] [value, argId]
        define symbol value
        withCurrentId defBlkId $ initVar Type.Bool (ConstBool True)

    x -> error (show x)


irGenerateCondition :: Inst.InstBuilderState -> ID -> AST.Expr -> IrGenerate ID
irGenerateCondition inst defBlkId (AST.Expr id) = case Inst.expressions inst Map.! id of
    AST.Match _ expr pat -> do
        val <- irGenerateExpr inst expr
        irGeneratePattern inst defBlkId pat val

    _ -> irGenerateExpr inst (AST.Expr id)

    x -> error (show x)



irGenerateExpr :: Inst.InstBuilderState -> AST.Expr -> IrGenerate ID
irGenerateExpr inst (AST.Expr id) = case Inst.expressions inst Map.! id of
    AST.Ident _ symbol          -> look symbol
    AST.Call _ (Type tid) exprs ->
        irGenerateCall (Inst.types inst Map.! tid) =<< mapM (irGenerateExpr inst) exprs

    AST.Reference _ expr -> irGenerateExpr inst expr

    AST.Float _ f  -> initVar F64 (ConstFloat f)
    AST.Int _ n    -> initVar I64 (ConstInt n)
    AST.Bool _ b   -> initVar Type.Bool (ConstBool b)
    AST.Char _ c   -> initVar Type.Char (ConstChar c)
    AST.String _ s -> initVar (Apply Slice Type.Char) (ConstString s)
    AST.Array _ es -> do
        let typ = (Inst.types inst Map.! id)
        base <- baseTypeOf typ
        case unfoldType base of
            (Slice, [t]) -> do
                id <- appendStmt . MakeSlice typ . map (ArgValue t) =<< mapM (irGenerateExpr inst) es
                addIdArg id (ArgValue typ id)
                return id

            x -> error (show x)

    x -> error (show x)


initVar :: Type -> Constant -> IrGenerate ID
initVar typ const = do
    symbol <- liftAstState $ findSymbol (Sym ["builtin", "builtinInit"])
    let callType = foldType [TypeDef symbol, typ]

    (Func, retType : []) <- unfoldType <$> baseTypeOf callType
    unless (typ == retType) (error $ show retType)

    callType' <- liftAstState (irGenerateInstance callType)
    unless (callType' == callType) (error "")

    id <- prependStmt $ Call callType' [ArgConst typ const]
    addIdArg id (ArgValue typ id)
    return id


call :: [String] -> [Type] -> [ID] -> IrGenerate ID
call xs ts argIds = do
    symbol <- liftAstState $ findSymbol (Sym xs)
    let callType = foldType (TypeDef symbol : ts)
    irGenerateCall callType argIds



------------------ Destroy Addon Below -------------------------------------------------------------

data DestroyFrame
    = DestroyLoop { frameIds :: [ID] }
    | DestroyBlock { frameIds :: [ID] }


data DestroyState = DestroyState
    { destroyStack :: [DestroyFrame]
    , isBuiltinStore :: Bool
    }


initDestroyState b = DestroyState
    { destroyStack = [DestroyBlock []]
    , isBuiltinStore = b
    }


newtype IrGenerateDestroy a = IrGenerateDestroy
    { unIrGenerateDestroy :: StateT DestroyState (StateT FuncIr (StateT ASTResolved (Except Error))) a }
    deriving (Functor, Applicative, Monad, MonadState DestroyState, MonadError Error)


instance MonadFail IrGenerateDestroy where
    fail = throwError . ErrorStr


instance MonadFuncIr IrGenerateDestroy where
    liftFuncIrState (StateT s) = IrGenerateDestroy $ lift $ state (runIdentity . s)


instance MonadTypeDefs IrGenerateDestroy where
    getTypeDefs = liftAstStateDestroy (gets typeDefsAll)


liftAstStateDestroy :: StateT ASTResolved (Except Error) a -> IrGenerateDestroy a
liftAstStateDestroy = IrGenerateDestroy . lift . lift


liftFuncIrMonad :: FuncIrMonad a -> IrGenerateDestroy a
liftFuncIrMonad f = do
    funcIr <- liftFuncIrState get
    case runExcept $ runStateT (unFuncIrMonad f) funcIr of
        Right (a, funcIr') -> liftFuncIrState (put funcIr') >> pure a
        Left err           -> error (show err)


runIrGenerateDestroy :: DestroyState -> FuncIr -> IrGenerateDestroy a
    -> StateT ASTResolved (Except Error) (a, FuncIr)
runIrGenerateDestroy destroyState funcIr f = do
    ((a, _), funcIr) <- runStateT (runStateT (unIrGenerateDestroy f) destroyState) funcIr
    return (a, funcIr)


-- add destroy for specific instance
irGenerateDestroyInst :: Type -> StateT ASTResolved (Except Error) ()
irGenerateDestroyInst instType = do
    funcIr <- gets (fromJust . Map.lookup instType . instantiations)
    let (TypeDef symbol, _) = unfoldType instType
    let isBuiltinStore = symbolsCouldMatch symbol $ Sym ["builtin", "store"]
    ((), funcIr') <- runIrGenerateDestroy (initDestroyState isBuiltinStore) initFuncIr (destroyIr funcIr)
    modify $ \s -> s { instantiations = Map.insert instType funcIr' (instantiations s) }
    

destroyIr :: FuncIr -> IrGenerateDestroy ()
destroyIr funcIr = do
    liftFuncIrState $ put $ funcIr
        { irStmts = Map.singleton 0 (Block [])
        , irCurrentId = 0
        }

    let Block ids = fromJust $ Map.lookup 0 (irStmts funcIr)
    mapM_ (destroyStmt funcIr) ids
    stmts <- liftFuncIrMonad (gets irStmts)
    let Block ids = stmts Map.! 0
    case ids of
        [] -> return ()
        xs -> case stmts Map.! (last xs) of
            Return _ -> return ()
            _        -> mapM_ destroy =<< destroyIdsAll


destroyStmt :: FuncIr -> Ir.ID -> IrGenerateDestroy ()
destroyStmt funcIr id = case fromJust $ Map.lookup id (irStmts funcIr) of
    Block ids -> do
        appendStmtWithId id (Block [])
        withCurrentId id $ mapM_ (destroyStmt funcIr) ids

    Return mid -> do
        case mid of
            Nothing -> mapM_ destroy =<< destroyIdsAll
            Just i  -> mapM_ destroy . filter (/=i) =<< destroyIdsAll
        void $ appendStmtWithId id (Return mid)

    EmbedC a b -> void $ appendStmtWithId id (EmbedC a b)

    Call callType b -> do
        let (TypeDef symbol, _) = unfoldType callType
        let isCopy = symbolsCouldMatch symbol (Sym ["builtin", "copy"])
        isBuiltinStore <- gets isBuiltinStore
        unless (isCopy && isBuiltinStore) $ do
            arg <- liftFuncIrMonad (getIdArg id)
            case arg of
                ArgValue typ i -> destroyAdd i
                _              -> return ()
        void $ appendStmtWithId id (Call callType b)

    If arg ids -> do
        pushStack (DestroyBlock [])
        appendStmtWithId id (If arg [])
        withCurrentId id $ do
            mapM_ (destroyStmt funcIr) ids
            mapM_ destroy =<< destroyIdsHead
        popStack

    Loop ids -> do
        pushStack (DestroyLoop [])
        appendStmtWithId id (Loop [])
        withCurrentId id $ do
            mapM_ (destroyStmt funcIr) ids
            mapM_ destroy =<< destroyIdsHead
        popStack

    Break -> do
        mapM_ destroy =<< destroyIdsLoop
        void $ appendStmtWithId id Break 

    MakeSlice a b -> void $ appendStmtWithId id (MakeSlice a b)

    With args ids -> do
        appendStmtWithId id (With args [])
        withCurrentId id $ mapM_ (destroyStmt funcIr) ids
                
    x -> error (show x)


pushStack :: DestroyFrame -> IrGenerateDestroy ()
pushStack frame = modify $ \s -> s { destroyStack = frame : destroyStack s }


popStack :: IrGenerateDestroy ()
popStack = modify $ \s -> s { destroyStack = tail (destroyStack s) }


destroyAdd :: Ir.ID -> IrGenerateDestroy ()
destroyAdd id = modify $ \s -> s
    { destroyStack = case (head $ destroyStack s) of
        DestroyLoop ids -> DestroyLoop (id : ids) : tail (destroyStack s)
        DestroyBlock ids -> DestroyBlock (id : ids) : tail (destroyStack s)
    }


destroyIdsAll :: IrGenerateDestroy [ID]
destroyIdsAll = gets (concat . map frameIds . destroyStack)


destroyIdsHead :: IrGenerateDestroy [ID]
destroyIdsHead = gets (frameIds . head . destroyStack)


destroyIdsLoop :: IrGenerateDestroy [ID]
destroyIdsLoop = gets (loopIds . destroyStack)
    where
        loopIds :: [DestroyFrame] -> [ID]
        loopIds (x : xs) = case x of
            DestroyLoop ids -> ids
            DestroyBlock ids -> ids ++ loopIds xs


destroy :: Ir.ID -> IrGenerateDestroy ()
destroy id = do
    symbol <- liftAstStateDestroy (findSymbol $ Sym ["builtin", "destroy"])
    arg <- liftFuncIrMonad (getIdArg id)
    case arg of
        ArgValue typ _ | isNoDestroy typ -> return ()
        ArgValue typ i -> do
            callType' <- liftAstStateDestroy $ irGenerateInstance (foldType [TypeDef symbol, typ])
            callId <- appendStmt $ Call callType' [ArgModify typ i]
            liftFuncIrMonad $ addIdArg callId (ArgValue Tuple callId)

        x -> error (show x)

    where
        isNoDestroy :: Type -> Bool
        isNoDestroy typ = case unfoldType typ of
            (TypeDef symbol, _) -> symbolsCouldMatch symbol (Sym ["compare", "Ordering"])
            (Sum, xs)           -> all isNoDestroy xs
            (Tuple, [])         -> True
            (Slice, _)          -> True
            (Bool, _)           -> True
            (I64, _)            -> True
            _                   -> False
       
