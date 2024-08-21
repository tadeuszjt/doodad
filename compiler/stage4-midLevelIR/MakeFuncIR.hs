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
    irParams <- forM (S.funcArgs $ S.funcHeader func) $ \param -> case param of
        -- TODO what about slice?
        S.Param _ symbol typ -> do
            id <- liftFuncIr generateId
            define symbol id
            liftFuncIr $ addType id typ Value
            return $ ParamIR (ArgID id) Value typ

        S.RefParam _ symbol typ -> do
            id <- liftFuncIr generateId
            define symbol id
            liftFuncIr $ addType id typ Ref
            return $ ParamIR (ArgID id) Ref typ

        x -> error (show x)

    irRetty <- case (S.funcRetty $ S.funcHeader func) of
        S.RefRetty t -> return (RettyIR Ref t)
        S.Retty t    -> return (RettyIR Value t)

    modify $ \s -> s { astRetty = Just (S.funcRetty $ S.funcHeader func) }

    makeStmt (S.funcStmt func)

    state <- get
    let funcIrHeader = FuncIrHeader
            { irAstHeader = S.funcHeader func
            , irRetty     = irRetty
            , irArgs      = irParams
            }
    return (funcIrHeader, irFunc state)


processCEmbed :: [(String, Symbol)] -> String -> DoM FuncIRState String
processCEmbed strMap str = case str of
    ('$':xs) -> do
        let ident = takeWhile (\c -> isAlpha c || isDigit c || c == '_') xs
        check (length ident > 0)     "invalid identifier following '$' token"
        check (isAlpha $ ident !! 0) "invalid identifier following '$' token"
        let rest = drop (length ident) xs

        case lookup ident strMap of
            Nothing -> error "here" 
            Just symbol -> do
                id <- look symbol
                (("$%" ++ show id) ++) <$> processCEmbed strMap rest

    (x:xs) -> (x:) <$> processCEmbed strMap xs
    []     -> return ""


makeStmt :: S.Stmt -> DoM FuncIRState ()
makeStmt statement = withPos statement $ case statement of
    S.Block stmts -> do
        id <- liftFuncIr $ appendStmt (Block [])
        withCurrentID id (mapM_ makeStmt stmts)

    S.EmbedC _ strMap str -> do
        str' <- processCEmbed strMap str
        uses <- mapM look (map snd strMap)
        void $ liftFuncIr $ appendStmt (EmbedC uses str')

    S.Let _ (S.PatAnnotated (S.PatIdent _ symbol) typ) (Just expr) Nothing -> do
        val <- makeVal expr
        case val of
            ArgConst _ _ -> define symbol =<< liftFuncIr (appendSSA typ Value (InitVar $ Just val))
            ArgID argId -> do
                resm <- liftFuncIr $ gets (Map.lookup argId . irStmts)
                case resm of
                    Just (SSA _ _ (Call _ _)) -> do define symbol argId

                    Just (SSA _ _ (MakeValueFromReference _)) -> do
                        id <- liftFuncIr $ appendSSA typ Value (InitVar Nothing)
                        idRef <- liftFuncIr $ appendSSA typ Ref (MakeReferenceFromValue id)
                        store (ArgID idRef) (ArgID argId)
                        void $ define symbol id

                    Nothing -> do -- probably an arg TODO
                        id <- liftFuncIr $ appendSSA typ Value (InitVar Nothing)
                        idRef <- liftFuncIr $ appendSSA typ Ref (MakeReferenceFromValue id)
                        store (ArgID idRef) (ArgID argId)
                        void $ define symbol id

                    x -> error (show x)


    S.Let _ (S.PatAnnotated (S.PatIdent _ symbol) typ) Nothing Nothing -> do
        define symbol =<< liftFuncIr (appendSSA typ Value (InitVar Nothing))

    S.Data _ symbol typ Nothing -> do
        define symbol =<< liftFuncIr (appendSSA typ Value (InitVar Nothing))

    S.Assign _ symbol expr -> do
        -- TODO remove Assign, it is bad
        arg <- makeVal expr
        (typ, _) <- liftFuncIr (getType arg)
        define symbol =<< liftFuncIr (appendSSA typ Value (InitVar $ Just arg))

    S.Return _ (Just expr) -> do
        Just retty <- gets astRetty
        case retty of
            S.RefRetty typ -> void $ (liftFuncIr . appendStmt . Return . ArgID) =<< makeRef expr
            S.Retty (Apply Type.Slice _) -> void $ (liftFuncIr . appendStmt . Return) =<< makeVal expr
            S.Retty typ -> do
                -- if this is a stack value, can return
                -- anything else needs to be created with store
                val <- makeVal expr
                case val of
                    ArgConst _ _ -> void $ liftFuncIr $ appendStmt (Return val)
                    ArgID argId -> do
                        mstmt <- liftFuncIr $ gets $ Map.lookup argId . irStmts
                        case mstmt of
                            Just (SSA _ _ (InitVar _)) -> void $ liftFuncIr $ appendStmt (Return val)
                            Just (SSA _ _ (Call _ _))  -> void $ liftFuncIr $ appendStmt (Return val)
                            _                -> do
                                id1 <- liftFuncIr $ appendSSA typ Value (InitVar Nothing)
                                id2 <- liftFuncIr $ appendSSA typ Ref (MakeReferenceFromValue id1)
                                store (ArgID id2) (ArgID argId)
                                void $ liftFuncIr $ appendStmt (Return $ ArgID id1)


                    x -> error (show x)
            x -> error (show x)

    S.Return _ Nothing     -> void $ liftFuncIr $ appendStmt ReturnVoid
    S.ExprStmt expr        -> void (makeVal expr)

    S.While _ expr (S.Block stmts) -> do
        id <- liftFuncIr $ appendStmt $ Loop []
        withCurrentID id $ do
            cnd <- makeVal expr
            ifId <- liftFuncIr $ appendStmt (If cnd [])
            elseId <- liftFuncIr $ appendStmt (Else [])
            withCurrentID elseId $ do
                liftFuncIr $ appendStmt Break
                
            mapM_ makeStmt stmts


    S.If _ expr (S.Block trueStmts) mfalse -> do
        arg <- makeVal expr
        id <- liftFuncIr $ appendStmt (If arg [])
        withCurrentID id (mapM_  makeStmt trueStmts)

        case mfalse of
            Nothing -> return ()
            Just (S.Block falseStmts) -> do
                elseId <- liftFuncIr $ appendStmt (Else [])
                withCurrentID elseId (mapM_ makeStmt falseStmts)
        

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
        unless (typ == exprType) (error $ "type mismatch: " ++ show (typ, exprType))

        case refType of
            Value -> return (ArgID id)
            -- TODO might be slow, create makeAny function?
            Ref   -> fmap ArgID $ liftFuncIr $ appendSSA typ Value (MakeValueFromReference id)


    S.Call _ funcType exprs -> do
        ast <- gets astResolved
        header <- fmap (S.funcHeader . fst) $ runDoMExcept ast (makeInstance funcType)
        unless (length exprs == length (S.funcArgs header)) (error "arg length mismatch")

        args <- forM (zip exprs $ S.funcArgs header) $ \(expr, arg) -> do
            case arg of
                S.RefParam _ argSymbol argType -> ArgID <$> makeRef expr
                S.Param    _ argSymbol argType -> makeVal expr

        case S.funcRetty header of
            S.Retty Void -> fmap ArgID $ liftFuncIr $ appendSSA Void Const (Call funcType args)
            S.Retty typ -> fmap ArgID $ liftFuncIr $ appendSSA typ Value (Call funcType args)
            S.RefRetty typ -> do
                fmap ArgID $ liftFuncIr $ appendSSA typ Value . MakeValueFromReference =<<
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
        unless (typ == exprType) (error "type mismatch")

        case refType of
            Value -> do
                liftFuncIr $ appendSSA typ Ref (MakeReferenceFromValue id)

            Ref -> return id
                
            x -> error (show x)

    S.Call _ funcType exprs -> do
        ast <- gets astResolved
        header <- fmap (S.funcHeader . fst) $ runDoMExcept ast (makeInstance funcType)
        unless (length exprs == length (S.funcArgs header)) (error "arg length mismatch")

        args <- forM (zip exprs $ S.funcArgs header) $ \(expr, arg) -> do
            case arg of
                S.RefParam _ argSymbol argType -> ArgID <$> makeRef expr
                S.Param    _ argSymbol argType -> makeVal expr

        case S.funcRetty header of
            -- TODO slice
            S.RefRetty typ -> liftFuncIr $ appendSSA typ Ref (Call funcType args)

            S.Retty typ -> do
                id <- liftFuncIr $ appendSSA typ Value (Call funcType args)
                liftFuncIr $ appendSSA typ Ref (MakeReferenceFromValue id)

            x -> error (show x)

    x -> error (show x)


store :: Arg -> Arg -> DoM FuncIRState ()
store dst src = do
    (t1, Ref) <- liftFuncIr $ getType dst
    (t2, Value) <- liftFuncIr $ getType src
    unless (t1 == t2) (error "type mismatch")

    -- get store feature symbol
    ast <- gets astResolved
    let xs = Map.keys $ Map.filterWithKey
            (\k v -> symbolsCouldMatch k $ Sym ["store", "store"])
            (typeDefsAll ast)
    storeSymbol <- case xs of
        [] -> fail "store::store undefined"
        [x] -> return x


    acq <- fmap fst $ runDoMExcept ast $ makeAcquireInstance (foldType [TypeDef storeSymbol, t1])
    unless (isJust acq) (fail $ "no store acquire for: " ++ show t1)

    let acqSymbol = S.funcSymbol $ S.funcHeader (fromJust acq)
    case S.funcArgs (S.funcHeader $ fromJust acq) of
        [S.RefParam _ _ _, S.Param _ _ _] -> do
            void $ liftFuncIr $ appendSSA Void Const $ Call (Apply (TypeDef storeSymbol) t1) [dst, src]
