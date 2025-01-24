{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Ir2
import ASTResolved
import Symbol
import AstBuilder
import FindFunc
import qualified AST
import qualified InstBuilder as Inst


data IrGenerateState = IrGenerateState
    { funcIr      :: FuncIr2
    , symbolTable :: Map.Map Symbol ID
    }


initIrGenerateState = IrGenerateState
    { funcIr      = initFuncIr2
    , symbolTable = Map.empty
    }



newtype IrGenerate a = IrGenerate
    { unIrGenerate :: StateT IrGenerateState (StateT ASTResolved (ExceptT Error IO)) a }
    deriving (Functor, Applicative, Monad, MonadState IrGenerateState, MonadIO, MonadError Error)


liftAstState :: StateT ASTResolved Identity a -> IrGenerate a
liftAstState (StateT s) = IrGenerate $ lift $ StateT $ pure . runIdentity . s

instance MonadFail IrGenerate where
    fail s = throwError (ErrorStr s)


runIrGenerate :: MonadIO m => IrGenerateState -> ASTResolved -> IrGenerate a -> m (Either Error ((a, IrGenerateState), ASTResolved))
runIrGenerate irGenerateState ast f =
    liftIO $ runExceptT $ runStateT (runStateT (unIrGenerate f) irGenerateState) ast


evalWithNewIrState :: IrGenerate a -> IrGenerate a
evalWithNewIrState f = fmap fst $ IrGenerate $ lift $ runStateT (unIrGenerate f) initIrGenerateState


define :: Symbol -> ID -> IrGenerate ()
define symbol id = do
    resm <- gets (Map.lookup symbol . symbolTable)
    when (isJust resm) (error "symbol already defined")
    modify $ \s -> s { symbolTable = Map.insert symbol id (symbolTable s) }


look :: Symbol -> IrGenerate ID
look symbol = do
    Just id <- gets (Map.lookup symbol . symbolTable)
    return id


modifyAst :: (ASTResolved -> ASTResolved) -> IrGenerate ()
modifyAst f = do
    liftAstState $ modify f



instance MonadFuncIr2 (IrGenerate) where
    liftFuncIrState (StateT s) = do
        (a, funcIr') <- gets (runIdentity . s . funcIr)
        modify $ \s -> s { funcIr = funcIr' }
        return a


instance MonadTypeDefs (IrGenerate) where
    getTypeDefs = liftAstState (gets typeDefsAll)


-- generate IR for all instances with no generics
irGenerateAst :: IrGenerate ()
irGenerateAst = do
    ast <- liftAstState get
    forM_ (instancesTop ast) $ \instSymbol -> case Map.lookup instSymbol (instancesAll ast) of
        Just (TopInst _ [] callType _ _ _) -> void $ irGenerateInstance callType
        Just (TopField _ [] callType _)    -> void $ irGenerateInstance callType
        Nothing                            -> error "no instance"
        _ -> return ()


-- ensure generated IR for function, return underlying call type
irGenerateInstance :: Type -> IrGenerate Type
irGenerateInstance callType = evalWithNewIrState $ do
    --liftIO $ putStrLn $ "irGenerateInstance: " ++ show callType
    ast <- liftAstState get
    case isNothing (Map.lookup callType $ instantiations ast) of
        True -> do
            let Right (Just topStmt) = runExcept (makeInstantiation ast callType)
            instType <- case topStmt of
                TopInst _ [] typ _ _ _ -> return typ
                TopField _ [] typ _    -> return typ

            callType' <- case instType == callType of
                False -> irGenerateInstance instType
                True -> case topStmt of
                    TopInst _ _ _ _ _ _ -> irGenerateTopInst topStmt >> return callType
                    TopField _ _ _ _    -> irGenerateTopField topStmt >> return callType

            return callType'
        False -> return callType


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
        [argType] -> void $ call ["builtin", "builtinStore"] [argType] [field, head argIds]
        xs -> do
            let tupType = ts !! i
            forM_ (zip xs [0..]) $ \(x, j) -> do
                ref <- call ["builtin", "builtinField"] [Size j, x, tupType] [field]
                call ["builtin", "builtinStore"] [x] [ref, argIds !! j]

    appendStmt (Return $ Just sum)

    funcIr <- gets funcIr
    modifyAst $ \s -> s
        { instantiations = Map.insert funcType funcIr (instantiations s)
        , instantiationsTop = Set.insert funcType (instantiationsTop s)
        }


            
irGenerateTopInst :: TopStmt -> IrGenerate ()
irGenerateTopInst (TopInst _ [] callType args isRef inst) = do
    --liftIO $ putStrLn $ "irGenerateTopInst: " ++ show callType
    let (TypeDef funcSymbol, _) = unfoldType callType
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
    funcIrHeader <- gets funcIr
    modifyAst $ \s -> s { instantiations = Map.insert callType funcIrHeader (instantiations s) }

    irGenerateStmt inst (AST.Stmt 0)

    funcIr <- gets funcIr
    modifyAst $ \s -> s
        { instantiations = Map.insert callType funcIr (instantiations s)
        , instantiationsTop = Set.insert callType (instantiationsTop s)
        }
            
            
irGenerateStmt :: Inst.InstBuilderState -> AST.Stmt -> IrGenerate ()
irGenerateStmt inst (AST.Stmt id) = case Inst.statements inst Map.! id of
    AST.ExprStmt expr -> void $ irGenerateExpr inst expr
    AST.Block ids     -> mapM_ (irGenerateStmt inst) ids
    AST.Return _ mexpr -> void $ appendStmt . Return =<< traverse (irGenerateExpr inst) mexpr
    AST.EmbedC _ strMap str -> do
        strMap' <- forM strMap $ \(s, symbol) -> do
            id' <- look symbol
            return (s, id')
        void $ appendStmt (EmbedC strMap' str)

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


        --appendStmt . MakeSlice (Inst.types inst Map.! id) =<<
        --mapM (irGenerateExpr inst) es

    x -> error (show x)


irGenerateCall :: Type -> [ID] -> IrGenerate ID
irGenerateCall callType argIds = do
    callType' <- irGenerateInstance callType
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


initVar :: Type -> Constant -> IrGenerate ID
initVar typ const = do
    symbol <- findSymbol (Sym ["builtin", "builtinInit"])
    let callType = foldType [TypeDef symbol, typ]

    (Func, retType : []) <- unfoldType <$> baseTypeOf callType
    unless (typ == retType) (error $ show retType)

    callType' <- irGenerateInstance callType
    unless (callType' == callType) (error "")

    id <- prependStmt $ Call callType' [ArgConst typ const]
    addIdArg id (ArgValue typ id)
    return id


call :: [String] -> [Type] -> [ID] -> IrGenerate ID
call xs ts argIds = do
    symbol <- findSymbol (Sym xs)
    let callType = foldType (TypeDef symbol : ts)
    irGenerateCall callType argIds


findSymbol :: Symbol -> IrGenerate Symbol
findSymbol symbol = do
    typeDefs <- liftAstState (gets typeDefsAll)
    xs <- fmap catMaybes $ forM (Map.keys typeDefs) $ \typeSymbol ->
        case symbolsCouldMatch symbol typeSymbol of
            True -> return (Just typeSymbol)
            False -> return Nothing
    case xs of
        [] -> error $ "symbol not found: " ++ prettySymbol symbol
        [x] -> return x
        xs  -> error "multiple symbols"


