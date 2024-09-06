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
    , rettyIr = RettyIR Const Void
    }


instance MonadFuncIR (DoM FuncIRState) where
    liftFuncIrState (StateT s) = do
        irFunc <- gets irFunc
        let (a, irFunc') = runIdentity (s irFunc)
        modify $ \s -> s { irFunc = irFunc' }
        return a


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


withCurrentID :: ID -> (DoM FuncIRState a) -> DoM FuncIRState ()
withCurrentID id f = do
    oldId <- liftFuncIrState (gets irCurrentId)
    liftFuncIrState $ modify $ \s -> s { irCurrentId = id }
    void f
    liftFuncIrState $ modify $ \s -> s { irCurrentId = oldId }


makeFuncIR :: S.Stmt -> DoM FuncIRState (FuncIrHeader, FuncIR)
makeFuncIR (S.FuncInst _ [] symbol args retty stmt) = do
    irParams <- forM args $ \param -> case param of
        S.Param _ symbol (Apply Type.Slice typ) -> do
            id <- generateId
            define symbol id
            addType id typ IR.Slice
            return (ParamIR IR.Slice typ)
        
        S.Param _ symbol typ -> do
            id <- generateId
            define symbol id
            addType id typ Value
            return $ ParamIR Value typ

        S.RefParam _ symbol typ -> do
            id <- generateId
            define symbol id
            addType id typ Ref
            return $ ParamIR Ref typ

        x -> error (show x)

    rettyIr <- case retty of
        S.Retty (Apply Type.Slice t) -> return (RettyIR IR.Slice t)
        S.RefRetty (Apply Type.Slice t) -> return (RettyIR IR.Slice t)
        S.RefRetty t -> return (RettyIR Ref t)
        S.Retty t    -> return (RettyIR Value t)

    modify $ \s -> s { rettyIr = rettyIr }

    case stmt of
        S.Block stmts -> mapM_ makeStmt stmts
        stmt          -> makeStmt stmt

    state <- get
    let funcIrHeader = FuncIrHeader
            { irRetty     = rettyIr
            , irArgs      = irParams
            , irFuncSymbol = symbol
            }
    return (funcIrHeader, irFunc state)



makeStmt :: S.Stmt -> DoM FuncIRState ()
makeStmt statement = withPos statement $ case statement of
    S.Block stmts -> do
        id <- appendStmt (Block [])
        withCurrentID id (mapM_ makeStmt stmts)

    S.EmbedC _ strMap str -> do
        strMap' <- forM strMap $ \(s, symbol) -> do
            id' <- look symbol
            return (s, id')
        uses <- mapM look (map snd strMap)
        void $ appendStmt (EmbedC strMap' str)

    S.Let _ (S.PatAnnotated (S.PatIdent _ symbol) typ) (Just expr) Nothing -> do
        val <- makeVal expr
        case val of
            ArgConst _ _ -> define symbol =<< appendSSA typ Value (InitVar $ Just val)
            ArgID argId -> do
                resm <- liftFuncIrState $ gets (Map.lookup argId . irStmts)
                case resm of
                    Just (SSA (Call _ _)) -> do define symbol argId
                    _ -> do
                        (ArgID id) <- copy (ArgID argId)
                        void $ define symbol id


    S.Let _ (S.PatAnnotated (S.PatIdent _ symbol) typ) Nothing Nothing -> do
        define symbol =<< appendSSA typ Value (InitVar Nothing)

    S.Assign _ symbol expr -> do
        case typeof expr of
            Apply Type.Slice t -> do
                arg@(ArgID id) <- makeSlice expr
                define symbol id
            _ -> do
                arg <- makeVal expr
                case arg of
                    ArgConst typ const -> define symbol =<< appendSSA typ Value (InitVar $ Just arg)
                    ArgID id -> define symbol id
                    x -> error (show x)

    S.Return _ (Just expr) -> do
        retty <- gets rettyIr
        case retty of
            RettyIR Value (Apply Type.Slice _) -> fail "val to slice"
            RettyIR Ref   (Apply Type.Slice _) -> fail "ref to slice"
            RettyIR IR.Slice typ -> void $ appendStmt . Return =<< makeSlice expr
            RettyIR Ref   typ -> void $ appendStmt . Return . ArgID =<< makeRef expr
            RettyIR Value typ -> void $ appendStmt . Return =<< makeVal expr
            x -> error (show x)

    S.Return _ Nothing     -> void $ appendStmt ReturnVoid
    S.ExprStmt expr        -> void (makeVal expr)

    S.While _ expr (S.Block stmts) -> do
        id <- appendStmt $ Loop []
        withCurrentID id $ do
            cnd <- makeVal expr

            trueBlkId <- generateId
            falseBlkId <- generateId
            appendStmt (If cnd trueBlkId falseBlkId)
            addStmt trueBlkId (Block [])
            addStmt falseBlkId (Block [])
            withCurrentID falseBlkId $ do
                appendStmt Break
                
            mapM_ makeStmt stmts


    S.If _ expr (S.Block trueStmts) mfalse -> do
        arg <- makeVal expr

        trueBlkId <- generateId
        falseBlkId <- generateId
        appendStmt (If arg trueBlkId falseBlkId)
        addStmt trueBlkId (Block [])
        addStmt falseBlkId (Block [])

        withCurrentID trueBlkId (mapM_ makeStmt trueStmts)
        case mfalse of
            Nothing -> return ()
            Just (S.Block falseStmts) -> do
                withCurrentID falseBlkId (mapM_ makeStmt falseStmts)
        

    x -> error (show x)


makeSlice :: S.Expr -> DoM FuncIRState Arg
makeSlice (S.AExpr exprType expression) = withPos expression $ case expression of
    S.String _ str -> do
        let Apply Type.Slice typ = exprType
        fmap ArgID $ appendSSA typ IR.Slice (MakeString str)

    S.Ident _ symbol -> do
        id <- look symbol
        (typ, IR.Slice) <- getType (ArgID id)
        return (ArgID id)

    S.Reference _ expr -> makeSlice expr

    S.Array _ exprs -> do
        let Apply Type.Slice t = exprType
        fmap ArgID $ appendSSA t IR.Slice . MakeSlice =<< mapM copy =<< mapM makeVal exprs


    S.Call _ funcType exprs -> do
        let (TypeDef funcSymbol, _) = unfoldType funcType

        ast <- gets astResolved
        resm <- fmap fst $ runDoMExcept ast (makeHeaderInstance funcType)
        irHeader <- case resm of
            Just x -> return x
            Nothing -> fail ("no instance for: " ++ show funcType)
        unless (length exprs == length (irArgs irHeader)) (error "arg length mismatch")

        args <- forM (zip exprs (irArgs irHeader)) $ \(expr, arg) -> do
            case arg of
                ParamIR _ (Apply Type.Slice _) -> error "there"
                ParamIR IR.Slice _ -> makeSlice expr
                ParamIR Ref _ -> ArgID <$> makeRef expr
                ParamIR Value argType -> do
                    if symbolsCouldMatch funcSymbol (Sym ["builtin", "copy"]) then makeVal expr
                    else copy =<< makeVal expr

        case irRetty irHeader of
            RettyIR IR.Slice t -> fmap ArgID $ appendSSA t IR.Slice (Call funcType args)
            x -> error ""


    x -> error (show x)


-- returns either a Value or a Const ID
makeVal :: S.Expr -> DoM FuncIRState Arg
makeVal (S.AExpr exprType expression) = withPos expression $ case expression of
    S.Bool _ b -> return (ArgConst exprType $ ConstBool b)
    S.Char _ c -> return (ArgConst exprType $ ConstChar c)
    S.Int _  n -> return (ArgConst exprType $ ConstInt n)
    S.Float _ f -> return $ ArgConst exprType (ConstFloat f)
    S.Reference _ expr -> makeVal expr

    S.Ident _ symbol -> do
        id <- look symbol
        (typ, refType) <- getType (ArgID id)
        case refType of
            Value -> return (ArgID id)
            Ref   -> fmap ArgID $ appendSSA typ Value (MakeValueFromReference (ArgID id))
            x -> fail "here"

--    S.Array _ exprs -> do
--        let (Type.Slice, [t]) = unfoldType exprType
--
--        appendSSA (foldType [Array, Size (length exprs), t]) Value (InitVar Nothing)
--        forM_ (zip exprs [0..]) $ \(expr, i) -> do
--            error "here"
--
--        error (show exprType)



    S.Call _ funcType exprs -> do
        let (TypeDef funcSymbol, _) = unfoldType funcType

        ast <- gets astResolved
        resm <- fmap fst $ runDoMExcept ast (makeHeaderInstance funcType)
        irHeader <- case resm of
            Just x -> return x
            Nothing -> fail ("no instance for: " ++ show funcType)
        unless (length exprs == length (irArgs irHeader)) (error "arg length mismatch")

        args <- forM (zip exprs (irArgs irHeader)) $ \(expr, arg) -> do
            case arg of
                ParamIR _ (Apply Type.Slice _) -> error "there"

                ParamIR IR.Slice _ -> makeSlice expr
                ParamIR Ref _ -> ArgID <$> makeRef expr
                ParamIR Value argType -> do
                    if symbolsCouldMatch funcSymbol (Sym ["builtin", "copy"]) then makeVal expr
                    else copy =<< makeVal expr

        case irRetty irHeader of
            RettyIR IR.Slice _ -> fail "slice return"
            RettyIR _ Void -> fmap ArgID $ appendSSA Void Const (Call funcType args)
            RettyIR Value typ -> fmap ArgID $ appendSSA typ Value (Call funcType args)
            RettyIR Ref typ -> do
                fmap ArgID $ appendSSA typ Value . MakeValueFromReference . ArgID =<<
                    appendSSA typ Ref (Call funcType args)

            x -> error (show x)

    x -> error (show x)
makeVal expr = withPos expr $ fail $ "unresolved: " ++ show expr


-- returns a Ref ID
makeRef :: S.Expr -> DoM FuncIRState ID
makeRef (S.AExpr exprType expression) = withPos expression $ case expression of
    S.Reference _ expr -> makeRef expr
    S.Ident _ symbol -> do
        id <- look symbol
        (typ, refType) <- getType (ArgID id)
        --TODO these may be different because of lower functions
        --unless (typ == exprType) (fail $ "type mismatch: " ++ show typ ++ ", " ++ show exprType)

        case refType of
            Value -> appendSSA typ Ref (MakeReferenceFromValue $ ArgID id)

            Ref -> return id
                
            x -> error (show x)

    S.Call _ funcType exprs -> do
        ast <- gets astResolved
        Just irHeader <- fmap fst $ runDoMExcept ast (makeHeaderInstance funcType)
        unless (length exprs == length (irArgs irHeader)) (error "arg length mismatch")

        args <- forM (zip exprs (irArgs irHeader)) $ \(expr, arg) -> do
            case arg of
                ParamIR IR.Slice _ -> makeSlice expr
                ParamIR Ref   _ -> ArgID <$> makeRef expr
                ParamIR Value _ -> makeVal expr

        case irRetty irHeader of
            RettyIR Ref typ -> appendSSA typ Ref (Call funcType args)
            RettyIR Value typ -> do
                id <- appendSSA typ Value (Call funcType args)
                appendSSA typ Ref (MakeReferenceFromValue $ ArgID id)
            x -> error (show x)

    x -> error (show x)


copy :: Arg -> DoM FuncIRState Arg
copy arg = do
    (t, refType) <- getType arg
    case refType of
        Const -> return ()
        Value -> return ()
        x -> error (show x)

    -- get copy feature symbol
    ast <- gets astResolved
    let xs = Map.keys $ Map.filterWithKey
            (\k v -> symbolsCouldMatch k $ Sym ["copy"])
            (typeDefsAll ast)
    copySymbol <- case xs of
        [] -> fail "builtin::copy undefined"
        [x] -> return x


    resm <- fmap fst $ runDoMExcept ast $ makeHeaderInstance (foldType [TypeDef copySymbol, t])
    [ParamIR Value _] <- case resm of
        Nothing -> fail ("no copy instance for: " ++ show t)
        Just irHeader -> return (irArgs irHeader)

    fmap ArgID $ appendSSA t Value $ Call (Apply (TypeDef copySymbol) t) [arg]
