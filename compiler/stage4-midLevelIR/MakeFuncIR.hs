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
    { idSupply  :: ID
    , idCurrent :: ID
    , statements :: Map.Map ID Stmt
    , symbolTable :: Map.Map Symbol ID
    , typeTable :: Map.Map ID (Type, RefType)

    , astResolved :: ASTResolved
    , astRetty    :: Maybe S.Retty
    }


initFuncIRState ast = FuncIRState
    { idSupply = 0
    , idCurrent = 0
    , statements = Map.singleton 0 (Block [])
    , symbolTable = Map.empty
    , typeTable = Map.empty
    , astResolved = ast
    , astRetty = Nothing
    }


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


lookType :: Arg -> DoM FuncIRState (Type, RefType)
lookType arg = do
    case arg of
        ArgConst typ _ -> return (typ, Const)
        ArgID id -> do
            resm <- gets (Map.lookup id . typeTable)
            unless (isJust resm) (fail $ "no type added for: " ++ show id)
            return (fromJust resm)
    


addType :: ID -> Type -> RefType -> DoM FuncIRState ()
addType id typ refType = do
    resm <- gets (Map.lookup id . typeTable)
    unless (isNothing resm) (fail $ "id already typed: " ++ show id)
    modify $ \s -> s { typeTable = Map.insert id (typ, refType) (typeTable s) }


generateID :: DoM FuncIRState ID
generateID = do
    id <- (+1) <$> gets idSupply
    modify $ \s -> s { idSupply = (idSupply s) + 1 }
    return id



appendStmt :: Stmt -> DoM FuncIRState ID
appendStmt stmt = do
    id <- generateID

    curId <- gets idCurrent
    curStmt <- gets $ (Map.! curId) . statements
    curStmt' <- case curStmt of
        Block xs -> return (Block $ xs ++ [id])
        Loop ids -> return (Loop $ ids ++ [id])
        If arg ids    -> return (If arg $ ids ++ [id])
        Else ids      -> return (Else $ ids ++ [id])
        x -> error (show x)

    modify $ \s -> s { statements = Map.insert curId curStmt' (statements s) }
    modify $ \s -> s { statements = Map.insert id stmt (statements s) }

    return id


withCurrentID :: ID -> (DoM FuncIRState a) -> DoM FuncIRState ()
withCurrentID id f = do
    oldId <- gets idCurrent
    modify $ \s -> s { idCurrent = id }
    void f
    modify $ \s -> s { idCurrent = oldId }
    



makeFuncIR :: S.Func -> DoM FuncIRState FuncIR
makeFuncIR func = do
    irParams <- forM (S.funcArgs $ S.funcHeader func) $ \param -> case param of
        -- TODO what about slice?
        S.Param _ symbol typ -> do
            id <- generateID
            define symbol id
            addType id typ Value
            return $ ParamIR (ArgID id) Value typ

        S.RefParam _ symbol typ -> do
            id <- generateID
            define symbol id
            addType id typ Ref
            return $ ParamIR (ArgID id) Ref typ

        x -> error (show x)

    irRetty <- case (S.funcRetty $ S.funcHeader func) of
        S.RefRetty t -> return (RettyIR Ref t)
        S.Retty t    -> return (RettyIR Value t)

    modify $ \s -> s { astRetty = Just (S.funcRetty $ S.funcHeader func) }

    makeStmt (S.funcStmt func)

    state <- get

    return $ FuncIR
        { irStatement = S.funcStmt func
        , irHeader    = S.funcHeader func

        , irRetty     = irRetty
        , irArgs      = irParams
        , irStmts     = statements state
        , irTypes     = typeTable state
        , irSymbols   = symbolTable state
        }

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
        id <- appendStmt (Block [])
        withCurrentID id (mapM_ makeStmt stmts)

    S.EmbedC _ strMap str -> do
        str' <- processCEmbed strMap str
        uses <- mapM look (map snd strMap)
        void $ appendStmt (EmbedC uses str')

    S.Let _ (S.PatAnnotated (S.PatIdent _ symbol) typ) Nothing Nothing -> do
        id <- appendStmt (InitVar Nothing)
        addType id typ Value
        define symbol id

    S.Data _ symbol typ Nothing -> do
        id <- appendStmt (InitVar Nothing) 
        addType id typ Value
        define symbol id

    S.Assign _ symbol expr -> do
        arg <- makeVal expr
        (typ, _) <- lookType arg
        id <- appendStmt (InitVar $ Just arg)
        addType id typ Value
        define symbol id

    S.Return _ (Just expr) -> do
        Just retty <- gets astRetty
        case retty of
            S.Retty typ -> void $ appendStmt . Return =<< makeVal expr
            S.RefRetty typ -> void $ appendStmt . Return . ArgID =<< makeRef expr
            x -> error (show x)



    S.Return _ Nothing     -> void $ appendStmt ReturnVoid
    S.ExprStmt expr        -> void (makeVal expr)

    S.While _ expr (S.Block stmts) -> do
        id <- appendStmt $ Loop []
        withCurrentID id $ do
            cnd <- makeVal expr
            ifId <- appendStmt (If cnd [])
            elseId <- appendStmt (Else [])
            withCurrentID elseId $ do
                appendStmt Break
                
            mapM_ makeStmt stmts


    S.If _ expr (S.Block trueStmts) mfalse -> do
        arg <- makeVal expr
        id <- appendStmt (If arg [])
        withCurrentID id (mapM_  makeStmt trueStmts)

        case mfalse of
            Nothing -> return ()
            Just (S.Block falseStmts) -> do
                elseId <- appendStmt (Else [])
                withCurrentID elseId (mapM_ makeStmt falseStmts)
        

    x -> error (show x)


-- returns either a Value or a Const ID
makeVal :: S.Expr -> DoM FuncIRState Arg
makeVal (S.AExpr exprType expression) = withPos expression $ case expression of

    S.Ident _ symbol -> do
        id <- look symbol
        (typ, refType) <- lookType (ArgID id)
        unless (typ == exprType) (error "type mismatch")

        case refType of
            Value -> return (ArgID id)
            Ref   -> do
                -- TODO might be slow, create makeAny function?
                id' <- appendStmt (MakeValueFromReference id)
                addType id' typ Value
                return (ArgID id')


    S.Call _ funcType exprs -> do
        ast <- gets astResolved
        header <- fmap (S.funcHeader . fst) $ runDoMExcept ast (makeInstance funcType)
        unless (length exprs == length (S.funcArgs header)) (error "arg length mismatch")

        args <- forM (zip exprs $ S.funcArgs header) $ \(expr, arg) -> do
            case arg of
                S.RefParam _ argSymbol argType -> ArgID <$> makeRef expr
                S.Param    _ argSymbol argType -> makeVal expr

        id <- appendStmt (Call funcType args)

        case S.funcRetty header of
            S.Retty Void -> do
                addType id Void Const
                return (ArgID id)

            S.Retty typ -> do
                addType id typ Value
                return (ArgID id)

            S.RefRetty typ -> do
                addType id typ Ref
                id' <- appendStmt (MakeValueFromReference id)
                addType id' typ Value
                return (ArgID id')

            -- TODO slice

            x -> error (show x)

    S.Bool _ b -> return (ArgConst exprType $ ConstBool b)
    S.Char _ c -> return (ArgConst exprType $ ConstChar c)
    S.Int _  n -> return (ArgConst exprType $ ConstInt n)
    S.String _ str -> do
        id <- appendStmt (MakeString str)
        addType id exprType Value
        return (ArgID id)

    S.Float _ f -> return $ ArgConst exprType (ConstFloat f)

    S.Field _ expr field -> do
        i <- case field of
            Left i -> return i
            Right fieldSymbol -> do
                (i, _) <- gets $ (Map.! fieldSymbol) . fieldsAll . astResolved
                return i

            x -> error (show x)


        arg <- makeVal expr
        (typ, refType) <- lookType arg
        case refType of
            Ref -> do
                -- TODO what about slice?
                let ArgID argId = arg 
                id <- appendStmt (MakeFieldFromRef argId i)
                addType id exprType Value
                return (ArgID id)

            Value -> do
                let ArgID argId = arg
                id <- appendStmt (MakeFieldFromVal argId i)
                addType id exprType Value
                return (ArgID id)
                
            x -> error (show x)

    S.Reference _ expr -> do
        -- TODO is this a fail?
        makeVal expr

    x -> error (show x)


-- returns a Ref ID
makeRef :: S.Expr -> DoM FuncIRState ID
makeRef (S.AExpr exprType expression) = withPos expression $ case expression of
    S.Reference _ expr -> makeRef expr
    S.Ident _ symbol -> do
        id <- look symbol
        (typ, refType) <- lookType (ArgID id)
        unless (typ == exprType) (error "type mismatch")

        case refType of
            Value -> do
                id' <- appendStmt (MakeReferenceFromValue id)
                addType id' typ Ref
                return id'

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

        id <- appendStmt (Call funcType args)

        case S.funcRetty header of
            -- TODO slice
            S.RefRetty typ -> do
                addType id typ Ref
                return id

            S.Retty typ -> do
                addType id typ Value
                id' <- appendStmt (MakeReferenceFromValue id)
                addType id' typ Ref
                return id'

            x -> error (show x)

    S.Field _ expr field -> do
        i <- case field of
            Left i -> return i
            Right fieldSymbol -> do
                (i, _) <- gets $ (Map.! fieldSymbol) . fieldsAll . astResolved
                return i

        argId <- makeRef expr
        (typ, refType) <- lookType (ArgID argId)
        case refType of
            Ref -> do
                id <- appendStmt (MakeFieldFromRef argId i)
                addType id exprType Ref
                return id
                
            x -> error (show x)

    x -> error (show x)
