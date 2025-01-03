{-# LANGUAGE FlexibleInstances #-}
module MakeFuncIR where

import Control.Monad.Identity
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe
import Data.Char

import Monad
import ASTResolved
import Symbol
import Type
import FindFunc
import Error
import IR
import AstBuilder
import InstBuilder hiding (ID, appendStmt, generateId)
import qualified AST as S


data FuncIRState = FuncIRState
    { symbolTable :: Map.Map Symbol ID
    , irFunc :: FuncIR
    , astResolved :: ASTResolved
    , rettyIr  :: RettyIR
    }


initFuncIRState ast = FuncIRState
    { symbolTable = Map.empty
    , irFunc = initFuncIr
    , astResolved = ast
    , rettyIr = RettyIR Value Tuple
    }


instance MonadFuncIR (DoM FuncIRState) where
    liftFuncIrState (StateT s) = do
        irFunc <- gets irFunc
        let (a, irFunc') = runIdentity (s irFunc)
        modify $ \s -> s { irFunc = irFunc' }
        return a

instance TypeDefs (DoM FuncIRState) where
    getTypeDefs = gets (typeDefsAll . astResolved)


define :: Symbol -> ID -> DoM FuncIRState ()
define symbol id = do
    resm <- gets (Map.lookup symbol . symbolTable)
    unless (isNothing resm) (fail $ "symbol already defined: " ++ prettySymbol symbol)
    modify $ \s -> s { symbolTable = Map.insert symbol id (symbolTable s) }


look :: Symbol -> DoM FuncIRState ID
look symbol = do
    resm <- gets (Map.lookup symbol . symbolTable)
    unless (isJust resm) (fail $ "symbol isn't defined: " ++ prettySymbol symbol)
    return (fromJust resm)


withCurrentID :: ID -> (DoM FuncIRState a) -> DoM FuncIRState a
withCurrentID id f = do
    oldId <- liftFuncIrState (gets irCurrentId)
    liftFuncIrState $ modify $ \s -> s { irCurrentId = id }
    a <- f
    liftFuncIrState $ modify $ \s -> s { irCurrentId = oldId }
    return a


makeFuncIR :: TopStmt -> DoM FuncIRState (FuncIrHeader, FuncIR)
makeFuncIR (TopField pos [] funcType i) = withPos pos $ do
    let (TypeDef funcSymbol, funcTypeArgs) = unfoldType funcType

    (Func, retType : argTypes) <- unfoldType <$> baseTypeOf funcType
    (Sum, ts)                  <- unfoldType <$> baseTypeOf retType

    argIds <- forM argTypes $ \argType -> do
        id <- generateId
        addType id argType Value
        return id

    sum <- appendSSA retType Value (InitVar Nothing)
    sumRef <- appendSSA retType IR.Ref (MakeReferenceFromValue $ ArgID sum)
    call (Sym ["builtin", "builtinSumReset"]) [retType] [ArgID sumRef, ArgConst I64 (ConstInt $ fromIntegral i)]

    field <- call (Sym ["builtin", "builtinField"]) [Size i, ts !! i, retType] [ArgID sumRef]
    case argTypes of
        [] -> return ()
        [argType] -> do
            void $ call (Sym ["builtin", "builtinStore"]) [argType] [field, ArgID $ head argIds]
        xs -> do
            let tupType = ts !! i
            forM_ (zip xs [0..]) $ \(x, j) -> do
                ref <- call (Sym ["builtin", "builtinField"]) [Size j, x, tupType] [field]
                void $ call (Sym ["builtin", "builtinStore"]) [x] [ref, ArgID $ argIds !! j]

    appendStmt (Return $ ArgID sum)

    let funcIrHeader = FuncIrHeader
            { irRetty = RettyIR Value retType
            , irArgs  = map (ParamIR Value) argTypes
            , irFuncSymbol = funcSymbol
            }
    state <- get
    return (funcIrHeader, irFunc state)



makeFuncIR (TopInst _ [] funcType args retty inst) = do
    let (TypeDef funcSymbol, _) = unfoldType funcType

    irParams <- forM args $ \param -> do
        (refType, typ, symbol) <- case param of
            S.Param _ symbol (Apply Type.Slice typ) -> return (IR.Slice, typ, symbol)
            S.Param _ symbol typ    -> return (Value, typ, symbol)
            S.RefParam _ symbol typ -> return (IR.Ref, typ, symbol)

        id <- generateId
        define symbol id
        addType id typ refType
        return (ParamIR refType typ)

    rettyIr <- case retty of
        S.Retty (Apply Type.Slice t) -> return (RettyIR IR.Slice t)
        S.RefRetty (Apply Type.Slice t) -> return (RettyIR IR.Slice t)
        S.RefRetty t -> return (RettyIR IR.Ref t)
        S.Retty t    -> return (RettyIR Value t)

    modify $ \s -> s { rettyIr = rettyIr }

    let Just (S.Block stmts) = Map.lookup 0 (statements inst)
    mapM_ (makeStmt inst) stmts

    state <- get
    let funcIrHeader = FuncIrHeader
            { irRetty     = rettyIr
            , irArgs      = irParams
            , irFuncSymbol = funcSymbol
            }
    return (funcIrHeader, irFunc state)


deref :: Arg -> DoM FuncIRState Arg
deref arg = do
    (typ, refType) <- getType arg
    case refType of
        Const -> return arg
        Value -> return arg
        -- IR.Slice -> return arg
        IR.Ref -> fmap ArgID $ appendSSA typ Value (MakeValueFromReference arg)


makeStmt :: InstBuilderState -> S.Stmt -> DoM FuncIRState ()
makeStmt inst (S.Stmt stmtId) = do
    let Just statement = Map.lookup stmtId (statements inst)
    case statement of
        S.Block stmts -> do
            -- id <- appendStmt (Block []) no need for block at this stage
            mapM_ (makeStmt inst) stmts

        S.EmbedC _ strMap str -> do
            strMap' <- forM strMap $ \(s, symbol) -> do
                id' <- look symbol
                return (s, id')
            uses <- mapM look (map snd strMap)
            id <- appendStmt (EmbedC strMap' str)
            addTextPos id (textPos statement)

        S.Let pos (S.Pattern patId) (Just expr) Nothing -> do
            let Just pattern = Map.lookup patId (patterns inst)
            case pattern of
                S.PatIdent _ symbol -> do
                    arg <- deref =<< makeExpr inst expr
                    id' <- case arg of
                        ArgConst t const -> appendSSA t Value (InitVar $ Just arg)
                        ArgID id -> do
                            funcIr <- gets irFunc
                            case Map.lookup id (irStmts funcIr) of
                                Just (SSA (Call _ _)) -> return id
                                Just (SSA (InitVar _)) -> return id
                                _ -> do
                                    ArgID id' <- copy arg
                                    return id'

                    addTextPos id' pos
                    define symbol id'


                _ -> do
                    curId <- getCurrentId
                    b <- makePattern curId inst (S.Pattern patId) =<< deref =<< makeExpr inst expr
                    void $ call (Sym ["assert", "assert"]) [] [b]

        S.Let pos (S.Pattern patId) Nothing Nothing -> do
            let Just (S.PatIdent _ symbol) = Map.lookup patId (patterns inst)
            let Just typ                   = Map.lookup patId (types inst)
            id <- appendSSA typ Value (InitVar Nothing)
            addTextPos id pos
            define symbol id

        S.Return _ (Just expr) -> do
            retty <- gets rettyIr
            arg <- makeExpr inst expr

            case retty of
                RettyIR _ (Apply Type.Slice _) -> fail "val to slice"
                RettyIR Value typ -> void $ appendStmt . Return =<< deref arg
                RettyIR IR.Slice typ -> do
                    (_, IR.Slice) <- getType arg
                    void $ appendStmt (Return arg)
                RettyIR IR.Ref   typ -> do
                    (typ, refType) <- getType arg
                    case refType of
                        IR.Ref -> void $ appendStmt (Return arg)
                    
                x -> error (show x)

        S.Return _ Nothing     -> void $ appendStmt $ Return (ArgConst Tuple (ConstTuple []))-- void $ appendStmt ReturnVoid
        S.ExprStmt expr        -> void $ makeExpr inst expr

        S.While pos expr (S.Stmt stmtId) -> do
            id <- appendStmt $ Loop []
            addTextPos id pos
            withCurrentID id $ do
                cnd <- makeCondition id inst expr
                not <- call (Sym ["boolean", "not"]) [Type.Bool] [cnd]

                ifId <- appendStmt (If not [])
                withCurrentID ifId (appendStmt Break)

                let Just (S.Block stmts) = Map.lookup stmtId (statements inst)
                mapM_ (makeStmt inst) stmts


        S.If pos expr (S.Stmt trueId) mfalse -> do
            curId <- getCurrentId
            arg <- makeCondition curId inst expr

            loopId <- appendStmt (Loop [])
            void $ withCurrentID loopId $ do
                -- make if
                ifId <- appendStmt (If arg [])
                let Just (S.Block trueStmts) = Map.lookup trueId (statements inst)
                withCurrentID ifId $ do
                    mapM_ (makeStmt inst) trueStmts
                    appendStmt Break

                case mfalse of
                    Nothing -> return ()
                    Just (S.Stmt falseId) -> do
                        let Just (S.Block falseStmts) = Map.lookup falseId (statements inst)
                        mapM_ (makeStmt inst) falseStmts

                appendStmt Break

        S.Switch pos expr cases -> do
            arg <- deref =<< makeExpr inst expr

            loopId <- appendStmt (Loop [])
            void $ withCurrentID loopId $ do
                forM_ cases $ \(pat, stmt) -> do
                    b <- makePattern loopId inst pat arg

                    ifId <- appendStmt (If b [])
                    withCurrentID ifId $ do
                        makeStmt inst stmt
                        appendStmt Break

                call (Sym ["assert", "assert"]) [] [ArgConst Type.Bool (ConstBool False)]
                appendStmt Break

        S.For pos expr mpat (S.Stmt blkId) -> do
            idx <- appendSSA I64 Value (InitVar Nothing)
            idxRef <- appendSSA I64 IR.Ref (MakeReferenceFromValue $ ArgID idx)

            loopId <- appendStmt (Loop [])
            void $ withCurrentID loopId $ do
                arg <- makeExpr inst expr

                end <- call (Sym ["for", "forEnd"]) [typeOfExpr inst expr] [arg]
                lte <- call (Sym ["builtin", "builtinLessThan"]) [Type.Bool] [ArgID idx, end]
                not <- call (Sym ["boolean", "not"]) [Type.Bool] [lte]
                ifId <- appendStmt (If not [])
                withCurrentID ifId (appendStmt Break)

                case mpat of
                    Nothing -> return ()
                    Just pat -> do
                        elem <- call (Sym ["for", "forAt"]) [typeOfPat inst pat, typeOfExpr inst expr]
                            [arg, ArgID idx]
                        b <- makePattern loopId inst pat =<< deref elem
                        not <- call (Sym ["boolean", "not"]) [Type.Bool] [b]
                        
                        ifId <- appendStmt (If not []) 
                        void $ withCurrentID ifId (appendStmt Break)


                let Just (S.Block stmts) = Map.lookup blkId (statements inst)
                mapM_ (makeStmt inst) stmts

                ip1 <- call (Sym ["arithmetic", "add"]) [I64] [ArgID idx, ArgConst I64 (ConstInt 1)]
                call (Sym ["builtin", "store"]) [I64] [ArgID idxRef, ip1]
                        



        x -> error (show x)



makeCondition :: ID -> InstBuilderState -> S.Expr -> DoM FuncIRState Arg
makeCondition defBlkId inst (S.Expr exprId) = case expressions inst Map.! exprId of
    S.Match _ (S.Expr id) pat -> do
        makePattern defBlkId inst pat =<< case expressions inst Map.! id of
            S.Match _ _ _ -> makeCondition defBlkId inst (S.Expr id)
            _             -> deref =<< makeExpr inst (S.Expr id)

    expr -> deref =<< makeExpr inst (S.Expr exprId)


makePattern :: ID -> InstBuilderState -> S.Pattern -> Arg -> DoM FuncIRState Arg
makePattern defBlkId inst (S.Pattern patId) arg = case patterns inst Map.! patId of
    S.PatIgnore _ -> return $ ArgConst Type.Bool (ConstBool True)

    S.PatLiteral expr@(S.Expr exprId) -> do
        expr' <- deref =<< makeExpr inst expr
        let Just exprType = Map.lookup exprId (types inst)
        call (Sym ["builtin", "builtinEqual"]) [exprType] [arg, expr']

    S.PatField _ symbol pat -> do
        Just (_, i) <- gets (Map.lookup symbol . fieldsAll . astResolved)

        let typ = typeOfPat inst (S.Pattern patId)

        en  <- call (Sym ["builtin", "builtinSumEnum"]) [typ] [arg]
        match <- call (Sym ["builtin", "builtinEqual"])   [I64] [en, ArgConst I64 (ConstInt $ fromIntegral i)]
        ifId <- appendStmt (If match [])
        withCurrentID ifId $ do
            ref <- appendSSA typ IR.Ref (MakeReferenceFromValue arg)
            field <- call (Sym ["builtin", "builtinField"]) [Size i, typeOfPat inst pat, typ] [ArgID ref]
            b <- makePattern defBlkId inst pat =<< deref field
            matchRef <- appendSSA Type.Bool IR.Ref (MakeReferenceFromValue match)
            call (Sym ["builtin", "store"]) [Type.Bool] [ArgID matchRef, b]

        return match


    S.PatIdent pos symbol -> do
        let patType = typeOfPat inst (S.Pattern patId)
        var <- withCurrentID defBlkId $
            prependSSA patType Value (InitVar Nothing)
        define symbol var

        ref <- appendSSA patType IR.Ref (MakeReferenceFromValue $ ArgID var)
        call (Sym ["builtin", "store"]) [patType] [ArgID ref, arg]
        return $ ArgConst Bool (ConstBool True)

    S.PatGuarded pos pat expr -> do
        patMatch <- makePattern defBlkId inst pat arg
        match <- appendSSA Type.Bool Value $ InitVar $ Just $ ArgConst Type.Bool (ConstBool False)

        ifId <- appendStmt (If patMatch [])
        withCurrentID ifId $ do
            matchRef <- appendSSA Type.Bool IR.Ref (MakeReferenceFromValue $ ArgID match)
            b <- makeCondition defBlkId inst expr
            call (Sym ["builtin", "store"]) [Type.Bool] [ArgID matchRef, b]

        return (ArgID match)


    S.PatTuple pos pats -> do
        match <- appendSSA Type.Bool Value $ InitVar $ Just $ ArgConst Type.Bool (ConstBool True)
        matchRef <- appendSSA Type.Bool IR.Ref (MakeReferenceFromValue $ ArgID match)
        loopId <- appendStmt (Loop [])

        withCurrentID loopId $ do
            forM_ (zip pats [0..]) $ \(pat@(S.Pattern fieldId), i) -> do

                let Just patType   = Map.lookup patId (types inst)
                let Just fieldType = Map.lookup fieldId (types inst)

                ref <- appendSSA patType IR.Ref (MakeReferenceFromValue arg)
                field <- call
                    (Sym ["tuple", "tuplePattern"])
                    [fieldType, Size (length pats), Size i, patType]
                    [ArgID ref]

                fieldVal <- appendSSA fieldType Value (MakeValueFromReference field)

                b <- makePattern defBlkId inst pat =<< copy (ArgID fieldVal)
                not <- call (Sym ["boolean", "not"]) [Type.Bool] [b]

                ifId <- appendStmt (If not [])
                withCurrentID ifId $ do
                    call (Sym ["builtin", "store"]) [Type.Bool] [ArgID matchRef, ArgConst Type.Bool (ConstBool False)]
                    appendStmt Break

            appendStmt Break

        return (ArgID match)

    x -> error (show x)


makeExpr :: InstBuilderState -> S.Expr -> DoM FuncIRState Arg
makeExpr inst (S.Expr exprId) = do
    let Just expression = Map.lookup exprId (expressions inst)
    let Just exprType   = Map.lookup exprId (types inst)
    withPos expression $ case expression of
        S.Bool _ b -> return $ ArgConst exprType (ConstBool b)
        S.Char _ c -> return $ ArgConst exprType (ConstChar c)
        S.Int _  n -> return $ ArgConst exprType (ConstInt n)
        S.Float _ f -> return $ ArgConst exprType (ConstFloat f)
        S.Reference _ expr -> makeExpr inst expr
        S.Ident _ symbol -> ArgID <$> look symbol

        S.Call pos typ exprs -> do
            let (TypeDef funcSymbol, funcTypeArgs) = unfoldType (typeOfType inst typ)
            call funcSymbol funcTypeArgs =<< mapM (makeExpr inst) exprs

        S.String _ str -> do
            let Apply Type.Slice typ = exprType
            fmap ArgID $ appendSSA typ IR.Slice (MakeString str)
        S.Array _ exprs -> do
            let Apply Type.Slice t = exprType
            fmap ArgID $ appendSSA t IR.Slice . MakeSlice =<< mapM copy =<<
                mapM (deref ) =<< mapM (makeExpr inst) exprs
        x -> error (show x)


copy :: Arg -> DoM FuncIRState Arg
copy arg = do
    (t, _) <- getType arg
    call (Sym ["builtin", "copy"]) [t] [arg]


call :: Symbol -> [Type] -> [Arg] -> DoM FuncIRState Arg
call symbol funcTypeArgs args = do
    ast <- gets astResolved

    symbol' <- case filter (symbolsCouldMatch symbol) (Map.keys (typeDefsAll ast)) of
        [] -> error "here"
        [x] -> return x
    let funcType = foldType (TypeDef symbol' : funcTypeArgs)

    resm <- fmap fst $ runDoMExcept ast (makeHeaderInstance funcType)
    irHeader <- case resm of
        Nothing -> fail $ "no instance: " ++ show funcType
        Just irHeader -> return irHeader

    unless (length (irArgs irHeader) == length args) (error "arg length mismatch")
    args' <- forM (zip args $ irArgs irHeader) $ \(arg, ParamIR paramType _) -> do
        (t, refType) <- getType arg

        case paramType of
            IR.Slice -> do
                unless (refType == IR.Slice) (error "slice only")
                return arg

            IR.Ref -> case refType of
                IR.Ref -> return arg
                IR.Value -> fmap ArgID $ appendSSA t IR.Ref (MakeReferenceFromValue arg)

            IR.Value -> case symbolsCouldMatch (Sym ["builtin", "copy"]) symbol' of
                True -> deref arg
                False -> copy =<< deref arg
                
            x -> fail (show x)

    case irRetty irHeader of
        RettyIR Value  t -> fmap ArgID $ appendSSA t Value (Call funcType args')
        RettyIR IR.Ref t -> fmap ArgID $ appendSSA t IR.Ref (Call funcType args')
        RettyIR IR.Slice t -> fmap ArgID $ appendSSA t IR.Slice (Call funcType args')
        x -> error (show x)


