module Collect where

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import Type
import Constraint
import Monad
import Error
import Control.Monad.State
import qualified SymTab
import Symbol
import ASTResolved
import FunctionFinder


type SymTab = SymTab.SymTab Symbol () Object
data Object
    = ObjVar Type
    | ObjConst S.Expr
    deriving (Show, Eq)

data CollectState
    = CollectState
        { symTab      :: SymTab
        , curRetty    :: Type
        , collected   :: Map.Map Constraint TextPos
        , defaults    :: Map.Map Constraint TextPos
        , curPos      :: TextPos
        , astResolved :: ASTResolved
        }

initCollectState astResolved = CollectState
    { symTab      = SymTab.initSymTab
    , curRetty    = Void
    , collected   = Map.empty
    , defaults    = Map.empty
    , curPos      = TextPos "" 0 0
    , astResolved = astResolved 
    }


collectPos :: (TextPosition t) => t -> DoM CollectState a -> DoM CollectState a
collectPos t m = withPos t $ do
    old <- gets curPos
    modify $ \s -> s { curPos = (textPos t) }
    r <- m
    modify $ \s -> s { curPos = old }
    return r


collect :: Constraint -> DoM CollectState ()
collect constraint =
    modify $ \s -> s { collected = Map.insert (constraint) (curPos s) (collected s) }

collectEq :: Type -> Type -> DoM CollectState ()
collectEq t1 t2 = collect $ ConsEq t1 t2

collectDefault :: Type -> Type -> DoM CollectState ()
collectDefault t1 t2 = do
    modify $ \s -> s { defaults = Map.insert (ConsEq t1 t2) (curPos s) (defaults s) }


look :: Symbol -> DoM CollectState Object
look symbol = do
    rm <- SymTab.lookup symbol () <$> gets symTab
    unless (isJust rm) (error $ show symbol ++ " undefined")
    return (fromJust rm)


define :: Symbol -> Object -> DoM CollectState ()
define symbol obj = do
    resm <- SymTab.lookupHead symbol () <$> gets symTab
    unless (isNothing resm) (error $ show symbol ++ " already defined")
    modify $ \s -> s { symTab = SymTab.insert symbol () obj (symTab s) }


collectAST :: Bool -> ASTResolved -> DoM CollectState ()
collectAST verbose ast = do
    --when verbose $ liftIO $ putStrLn "collecting..."
    forM (Map.toList $ constDefs ast) $ \(symbol, expr) -> do
        define symbol (ObjConst expr)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) ->
        when (funcTypeArgs body == []) $
            collectFuncDef symbol body


collectFuncDef :: Symbol -> FuncBody -> DoM CollectState ()
collectFuncDef symbol body = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }
    oldRetty <- gets curRetty
    modify $ \s -> s { curRetty = funcRetty body }
    forM (funcParams body) $ \(S.Param _ symbol t) -> define symbol (ObjVar t)
    forM_ (funcArgs body) $ \(S.Param _ symbol t) -> define symbol (ObjVar t)
    collectStmt (funcStmt body)
    modify $ \s -> s { curRetty = oldRetty }
    --collectDefault (funcRetty body) Void
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }



collectStmt :: S.Stmt -> DoM CollectState ()
collectStmt stmt = collectPos stmt $ case stmt of
    S.Increment _ expr       -> collectExpr expr
    S.Typedef _ _ _ _        -> return ()
    S.FuncDef _ _ _ _ _ _ _  -> return ()
    S.EmbedC _ _             -> return ()
    S.Block stmts            -> mapM_ collectStmt stmts
    S.Const _ symbol expr    -> define symbol (ObjConst expr)
    S.ExprStmt expr -> do
        collectExpr expr
        collectDefault (typeof expr) Void

    S.Return _ mexpr -> do
        retty <- gets curRetty
        case mexpr of
            Nothing -> collectEq Void retty
            Just expr -> do
                collectEq (typeof expr) retty
                collectExpr expr

    S.If _ expr blk melse -> do
        collect $ ConsBase Bool (typeof expr)
        collectExpr expr
        collectStmt blk
        maybe (return ()) collectStmt melse

    S.Let _ pattern expr mblk -> do
        collectEq (typeof pattern) (typeof expr)
        collectPattern pattern
        collectExpr expr
        when (isJust mblk) $ collectStmt (fromJust mblk)

    -- only tables use +=, must be table
    S.SetOp _ S.PlusEq expr1 expr2@(S.AExpr t2 (S.Tuple _ es)) -> do
        case es of
            [] -> fail "what"
            [e] -> do
                collectEq (typeof expr1) (typeof expr2)
                collectExpr expr1
                collectExpr expr2
--            es -> do
--                gts <- replicateM (length es) genType
--                collectBase (typeof expr1) $ Table gts
--                forM_ (zip es gts) $ \(e, t) -> do
--                    collectEq (typeof e) $ Table [t]
--                    collectExpr e
--                collectExpr expr1
--                collectEq (typeof expr2) (typeof expr1)

    S.SetOp _ op expr1 expr2 -> do
        collectEq (typeof expr1) (typeof expr2)
        collectExpr expr1
        collectExpr expr2

    S.While _ expr blk -> do
        collectExpr expr
        collect $ ConsBase Bool (typeof expr)
        collectStmt blk

    S.Switch p expr cases -> do
        collectExpr expr
        forM_ cases $ \(pat, stmt) -> do
            collectEq (typeof pat) (typeof expr)
            collectPattern pat
            collectStmt stmt

    S.For p expr mpat blk -> do
        when (isJust mpat) $ do
            collect $ ConsSubscript (typeof expr) (typeof $ fromJust mpat)
            collectPattern (fromJust mpat)

        collectExpr expr
        collectStmt blk

    S.Data p symbol typ mexpr -> do
        define symbol (ObjVar typ)
        maybe (return ()) (collectEq typ . typeof) mexpr
        maybe (return ()) collectExpr mexpr
        
    _ -> error (show stmt)


-- collectPattern pattern <with this type of expression trying to match>
collectPattern :: S.Pattern -> DoM CollectState ()
collectPattern (S.PatAnnotated pattern patType) = collectPos pattern $ case pattern of
    S.PatIgnore pos        -> return ()
    S.PatNull _            -> return ()
    S.PatIdent _ symbol    -> do
        define symbol (ObjVar patType)

    S.PatTypeField _ t pat -> do
        collectEq patType t
        collectPattern pat

    S.PatLiteral expr -> do 
        collectEq patType (typeof expr)
        collectExpr expr

    S.PatGuarded _ pat expr -> do
        collect $ ConsBase Bool (typeof expr)
        collectPattern pat
        collectExpr expr

    S.PatField _ symbol pats -> do
        ast <- gets astResolved
        candidates <- fmap fst $ runDoMExcept ast (findCtorCandidates symbol)
        symbol' <- case candidates of
            [s] -> return s
            xs  -> error $ "PatField candidates for: " ++ show symbol ++ " " ++ show xs

        (s, i) <- mapGet symbol' . ctorDefs =<< gets astResolved
        collect $ ConsAdtField patType i (map typeof pats)
        mapM_ collectPattern pats

    S.PatTuple _ pats -> do
        collectDefault patType (Tuple $ Record $ map typeof pats)
        collect $ ConsTuple patType (map typeof pats)
        mapM_ collectPattern pats

    S.PatArray _ pats -> do
        --gt <- genType
        --mapM_ (\p -> collectPattern p gt) pats
        error "here"
        --collect $ ConsMember typ 0 gt

    S.PatAnnotated pat t -> do
        collectEq t patType
        collectEq t (typeof pat)
        collectPattern pat

    S.PatRecord _ pats -> do
        collect $ ConsRecord patType (map typeof pats)
        mapM_ collectPattern pats
        
    _ -> error $ show pattern


collectCall :: Type -> [S.Expr] -> Symbol -> [S.Expr] -> DoM CollectState ()
collectCall exprType params symbol args = do -- can be resolved or sym
    let callHeader = FuncHeader [] (map typeof params) symbol (map typeof args) exprType
    ast <- gets astResolved
    candidates <- fmap fst $ runDoMExcept ast (findCandidates callHeader)
    case candidates of
        [symbol] | isGenericFunction symbol ast -> return ()
        [symbol] | isNonGenericFunction symbol ast -> do
            let header = getFunctionHeader symbol ast
            collectEq exprType (returnType header)
            zipWithM_ collectEq (map typeof params) (paramTypes header)
            zipWithM_ collectEq (map typeof args) (argTypes header)

        [symbol] | isCtor symbol ast -> return ()

        _ -> return ()

    mapM_ collectExpr params
    mapM_ collectExpr args


collectExpr :: S.Expr -> DoM CollectState ()
collectExpr (S.AExpr exprType expression) = collectPos expression $ case expression of
    S.Call _ ps s exprs -> collectCall exprType ps s exprs
    S.Prefix _ op expr  -> collectEq exprType (typeof expr) >> collectExpr expr
    S.Int _ _           -> collectDefault exprType I64
    S.Float _ _         -> collectDefault exprType F64
    S.String _ _        -> do
        collect (ConsBase exprType String)
        collectDefault exprType String
    S.Null _            -> return ()

    S.Construct _ symbol args -> do
        mapM_ collectExpr args
        (typeSymbol, i)    <- mapGet symbol =<< gets (ctorDefs . astResolved)
        (generics, ADT ts) <- mapGet typeSymbol =<< gets (typeFuncs . astResolved)
        case exprType of
            Type _ -> return ()

            TypeApply s _ | s == typeSymbol -> do
                case args of
                    []    -> unless ( (ts !! i) == Void ) (error "type wasn't void")
                    [arg] -> collect $ ConsAdtField exprType i [typeof arg]
                    args  -> collect $ ConsAdtField exprType i (map typeof args)

            _ -> return () -- TODO
            _ -> error (show exprType)

    S.Builtin _ sym args -> do 
        case sym of
            "conv"  -> return ()
            "len"   -> do
                collect (ConsBase exprType I64)
                collectDefault exprType I64
            "print" -> collectEq exprType Void
        mapM_ collectExpr args

    S.Char _ c -> do
        collect (ConsBase exprType Char)
        collectDefault exprType Char

    S.Bool _ b -> do
        collect (ConsBase exprType Bool)
        collectDefault exprType Bool

    S.Ident _ symbol -> do
        obj <- look symbol 
        case obj of
            ObjVar typ -> collectEq typ exprType
            ObjConst e -> do -- special!
                return ()
--                count <- gets typeSupply
--                (e', count') <- runDoMExcept count (annotate e)
--                modify $ \s -> s { typeSupply = count' }
--                collectEq (typeof e') exprType
--                collectExpr e'
                
    S.Infix _ op e1 e2 -> do
        case op of
            _ | op `elem` [S.Plus, S.Minus, S.Times, S.Divide, S.Modulo] -> do
                collectEq exprType (typeof e1)
            _ | op `elem` [S.LT, S.GT, S.LTEq, S.GTEq, S.EqEq, S.NotEq]  -> do
                collect $ ConsBase exprType Bool
                collectDefault exprType Bool
            _ | op `elem` [S.AndAnd, S.OrOr] -> do
                collect $ ConsBase exprType Bool
                collectEq exprType (typeof e1)
            _ -> return ()
                    
        collectEq (typeof e1) (typeof e2)
        collectExpr e1
        collectExpr e2

    S.Subscript _ e1 e2 -> do
        collect $ ConsSubscript (typeof e1) exprType
        collectExpr e1
        collectExpr e2
        --collectDefault (typeof e2) I64

    S.Tuple _ exprs -> do
        collect $ ConsTuple exprType (map typeof exprs)
        collectDefault exprType $ Tuple $ Record (map typeof exprs)
        mapM_ collectExpr exprs

    S.Field _ e symbol -> do
        collect $ ConsField (typeof e) symbol exprType
        collectExpr e

    S.Match _ e p -> do
        collectEq (typeof p) (typeof e)
        collectPattern p
        collectExpr e
        collectDefault exprType Bool

    S.RecordAccess _ expr -> do
        collect $ ConsRecordAccess exprType (typeof expr) 
        collectExpr expr

    S.Range _ me me1 me2 -> do
        when (isJust me) $ collectExpr (fromJust me)

        when (isJust me1 && isJust me2) $ do
            collectEq (typeof $ fromJust me1) (typeof $ fromJust me2)
        when (isJust me1) $ do
            collect $ ConsBase (Range $ typeof $ fromJust me1) exprType
            collectDefault (Range $ typeof $ fromJust me1) exprType
            collectExpr (fromJust me1)
        when (isJust me2) $ do
            collect $ ConsBase (Range $ typeof $ fromJust me2) exprType
            collectDefault (Range $ typeof $ fromJust me2) exprType
            collectExpr (fromJust me2)
        when (isNothing me1 && isNothing me2) $ do
            collectDefault (Range I64) exprType

--    S.Array _ es -> do
--        forM_ es $ \e -> do 
--            collectElem exprType (typeof e)
--            collectEq (typeof e) (typeof $ head es)
--
--        collectDefault exprType $ Array (length es) (typeof $ head es)
--
--        mapM_ collectExpr es

    _ -> error (show expression)
