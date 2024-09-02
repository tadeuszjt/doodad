module MakeFuncIR where

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
    , astRetty    :: Maybe S.Retty
    }


initFuncIRState ast = FuncIRState
    { symbolTable = Map.empty
    , irFunc = initFuncIr
    , astResolved = ast
    , astRetty = Nothing
    }


liftFuncIr :: DoM FuncIR a -> DoM FuncIRState a
liftFuncIr f = do
    fn <- gets irFunc
    (a, fn') <- runDoMExcept fn f
    modify $ \s -> s { irFunc = fn' }
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
    oldId <- liftFuncIr (gets irCurrentId)
    liftFuncIr $ modify $ \s -> s { irCurrentId = id }
    void f
    liftFuncIr $ modify $ \s -> s { irCurrentId = oldId }


makeFuncIR :: S.Func -> DoM FuncIRState (FuncIrHeader, FuncIR)
makeFuncIR func = do
    irParams <- forM (S.funcArgs func) $ \param -> case param of
        -- TODO what about slice?
        S.Param _ symbol typ -> do
            id <- liftFuncIr generateId
            define symbol id
            liftFuncIr $ addType id typ Value
            return $ ParamIR Value typ

        S.RefParam _ symbol typ -> do
            id <- liftFuncIr generateId
            define symbol id
            liftFuncIr $ addType id typ Ref
            return $ ParamIR Ref typ

        x -> error (show x)

    irRetty <- case (S.funcRetty func) of
        S.RefRetty t -> return (RettyIR Ref t)
        S.Retty t    -> return (RettyIR Value t)

    modify $ \s -> s { astRetty = Just (S.funcRetty func) }

    case S.funcStmt func of
        S.Block stmts -> mapM_ makeStmt stmts
        stmt          -> makeStmt stmt

    state <- get
    let funcIrHeader = FuncIrHeader
            { irRetty     = irRetty
            , irArgs      = irParams
            , irFuncSymbol = S.funcSymbol func
            }
    return (funcIrHeader, irFunc state)



makeStmt :: S.Stmt -> DoM FuncIRState ()
makeStmt statement = withPos statement $ case statement of
    S.Block stmts -> do
        id <- liftFuncIr $ appendStmt (Block [])
        withCurrentID id (mapM_ makeStmt stmts)

    S.EmbedC _ strMap str -> do
        strMap' <- forM strMap $ \(s, symbol) -> do
            id' <- look symbol
            return (s, id')
        uses <- mapM look (map snd strMap)
        void $ liftFuncIr $ appendStmt (EmbedC strMap' str)

    S.Let _ (S.PatAnnotated (S.PatIdent _ symbol) typ) (Just expr) Nothing -> do
        val <- makeVal expr
        case val of
            ArgConst _ _ -> define symbol =<< liftFuncIr (appendSSA typ Value (InitVar $ Just val))
            ArgID argId -> do
                resm <- liftFuncIr $ gets (Map.lookup argId . irStmts)
                case resm of
                    Just (SSA (Call _ _)) -> do define symbol argId
                    _ -> do
                        (ArgID id) <- copy (ArgID argId)
                        void $ define symbol id


    S.Let _ (S.PatAnnotated (S.PatIdent _ symbol) typ) Nothing Nothing -> do
        define symbol =<< liftFuncIr (appendSSA typ Value (InitVar Nothing))

    S.Data _ symbol typ Nothing -> do
        define symbol =<< liftFuncIr (appendSSA typ Value (InitVar Nothing))

    S.Assign _ symbol expr -> do
        arg <- makeVal expr
        case arg of
            ArgConst typ const -> define symbol =<< liftFuncIr (appendSSA typ Value $ InitVar $ Just arg)
            ArgID id -> define symbol id
            x -> error (show x)

    S.Return _ (Just expr) -> do
        Just retty <- gets astRetty
        case retty of
            S.RefRetty typ -> void $ (liftFuncIr . appendStmt . Return . ArgID) =<< makeRef expr
            S.Retty (Apply Type.Slice _) -> void $ (liftFuncIr . appendStmt . Return) =<< makeVal expr
            S.Retty typ -> void $ liftFuncIr . appendStmt . Return =<< makeVal expr
            x -> error (show x)

    S.Return _ Nothing     -> void $ liftFuncIr $ appendStmt ReturnVoid
    S.ExprStmt expr        -> void (makeVal expr)

    S.While _ expr (S.Block stmts) -> do
        id <- liftFuncIr $ appendStmt $ Loop []
        withCurrentID id $ do
            cnd <- makeVal expr

            trueBlkId <- liftFuncIr generateId
            falseBlkId <- liftFuncIr generateId
            liftFuncIr $ appendStmt (If cnd trueBlkId falseBlkId)
            liftFuncIr $ addStmt trueBlkId (Block [])
            liftFuncIr $ addStmt falseBlkId (Block [])
            withCurrentID falseBlkId $ do
                liftFuncIr $ appendStmt Break
                
            mapM_ makeStmt stmts


    S.If _ expr (S.Block trueStmts) mfalse -> do
        arg <- makeVal expr

        trueBlkId <- liftFuncIr $ generateId
        falseBlkId <- liftFuncIr $ generateId
        liftFuncIr $ appendStmt (If arg trueBlkId falseBlkId)
        liftFuncIr $ addStmt trueBlkId (Block [])
        liftFuncIr $ addStmt falseBlkId (Block [])

        withCurrentID trueBlkId (mapM_ makeStmt trueStmts)
        case mfalse of
            Nothing -> return ()
            Just (S.Block falseStmts) -> do
                withCurrentID falseBlkId (mapM_ makeStmt falseStmts)
        

    x -> error (show x)


-- returns either a Value or a Const ID
makeVal :: S.Expr -> DoM FuncIRState Arg
makeVal (S.AExpr exprType expression) = withPos expression $ case expression of
    S.Bool _ b -> return (ArgConst exprType $ ConstBool b)
    S.Char _ c -> return (ArgConst exprType $ ConstChar c)
    S.Int _  n -> return (ArgConst exprType $ ConstInt n)
    S.String _ str -> fmap ArgID $ liftFuncIr $ appendSSA exprType Value (MakeString str)
    S.Float _ f -> return $ ArgConst exprType (ConstFloat f)
    S.Reference _ expr -> makeVal expr

    S.Ident _ symbol -> do
        id <- look symbol
        (typ, refType) <- liftFuncIr (getType (ArgID id))
        --TODO these may be different because of lower functions
        --unless (typ == exprType) (fail $ "type mismatch: " ++ show typ ++ ", " ++ show exprType)
        --unless (typ == exprType) (error $ "type mismatch: " ++ show (typ, exprType))

        case refType of
            Value -> return (ArgID id)
            -- TODO might be slow, create makeAny function?
            Ref   -> fmap ArgID $ liftFuncIr $ appendSSA typ Value (MakeValueFromReference (ArgID id))

    S.Array _ exprs -> do
        let (Type.Slice, [t]) = unfoldType exprType

        liftFuncIr $ appendSSA (foldType [Array, Size (length exprs), t]) Value (InitVar Nothing)
        forM_ (zip exprs [0..]) $ \(expr, i) -> do
            error "here"

        error (show exprType)



    S.Call _ funcType exprs -> do
        let (TypeDef funcSymbol, _) = unfoldType funcType

        ast <- gets astResolved
        Just irHeader <- fmap fst $ runDoMExcept ast (makeHeaderInstance funcType)
        unless (length exprs == length (irArgs irHeader)) (error "arg length mismatch")

        args <- forM (zip exprs (irArgs irHeader)) $ \(expr, arg) -> do
            case arg of
                ParamIR Ref _ -> ArgID <$> makeRef expr
                ParamIR Value (Apply Type.Slice _) -> makeVal expr
                ParamIR Value argType -> do
                    if symbolsCouldMatch funcSymbol (Sym ["builtin", "copy"]) then makeVal expr
                    else copy =<< makeVal expr

        case irRetty irHeader of
            RettyIR _ Void -> fmap ArgID $ liftFuncIr $ appendSSA Void Const (Call funcType args)
            RettyIR Value typ -> fmap ArgID $ liftFuncIr $ appendSSA typ Value (Call funcType args)
            RettyIR Ref typ -> do
                fmap ArgID $ liftFuncIr $ appendSSA typ Value . MakeValueFromReference . ArgID =<<
                    appendSSA typ Ref (Call funcType args)

            -- TODO slice

            x -> error (show x)

    x -> error (show x)


-- returns a Ref ID
makeRef :: S.Expr -> DoM FuncIRState ID
makeRef (S.AExpr exprType expression) = withPos expression $ case expression of
    S.Reference _ expr -> makeRef expr
    S.Ident _ symbol -> do
        id <- look symbol
        (typ, refType) <- liftFuncIr $ getType (ArgID id)
        --TODO these may be different because of lower functions
        --unless (typ == exprType) (fail $ "type mismatch: " ++ show typ ++ ", " ++ show exprType)

        case refType of
            Value -> do
                liftFuncIr $ appendSSA typ Ref (MakeReferenceFromValue $ ArgID id)

            Ref -> return id
                
            x -> error (show x)

    S.Call _ funcType exprs -> do
        ast <- gets astResolved
        Just irHeader <- fmap fst $ runDoMExcept ast (makeHeaderInstance funcType)
        unless (length exprs == length (irArgs irHeader)) (error "arg length mismatch")

        args <- forM (zip exprs (irArgs irHeader)) $ \(expr, arg) -> do
            case arg of
                ParamIR Ref   _ -> ArgID <$> makeRef expr
                ParamIR Value _ -> makeVal expr

        case irRetty irHeader of
            -- TODO slice
            RettyIR Ref typ -> liftFuncIr $ appendSSA typ Ref (Call funcType args)
            RettyIR Value typ -> do
                id <- liftFuncIr $ appendSSA typ Value (Call funcType args)
                liftFuncIr $ appendSSA typ Ref (MakeReferenceFromValue $ ArgID id)

            x -> error (show x)

    x -> error (show x)


copy :: Arg -> DoM FuncIRState Arg
copy arg = do
    (t, refType) <- liftFuncIr $ getType arg
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

    fmap ArgID $ liftFuncIr $ appendSSA t Value $ Call (Apply (TypeDef copySymbol) t) [arg]
