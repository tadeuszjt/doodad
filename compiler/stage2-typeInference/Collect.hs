module Collect where

import Data.Maybe
import Data.List
import qualified Data.Map as Map

import AST
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
collectEq t1 t2 = collect (ConsEq t1 t2)

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


collectAST :: Prelude.Bool -> ASTResolved -> DoM CollectState ()
collectAST verbose ast = do
    --when verbose $ liftIO $ putStrLn "collecting..."
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) ->
        when (funcGenerics body == []) $
            collectFuncDef symbol body


collectCall :: Type -> Symbol -> [Expr] -> DoM CollectState ()
collectCall exprType symbol args = do -- can be resolved or sym
    ast <- gets astResolved
    candidates <- if symbolIsResolved symbol then
        return [symbol]
    else fmap fst $ runDoMExcept ast $ findCandidates $
        CallHeader Nothing symbol (map typeof args) exprType

    case candidates of
        [symbol] | isGenericFunction symbol ast -> return ()
        [symbol] | isNonGenericFunction symbol ast -> do
            let body = getFunctionBody symbol ast
            collectEq exprType (funcRetty body)
            zipWithM_ collectEq (map typeof args)  (map typeof $ funcArgs body)

        _ -> return ()


collectFuncDef :: Symbol -> FuncBody -> DoM CollectState ()
collectFuncDef symbol body = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }
    oldRetty <- gets curRetty
    modify $ \s -> s { curRetty = funcRetty body }
    forM (funcParams body) $ \(Param _ symbol t) -> error ""
    forM_ (funcArgs body) $ \(Param _ symbol t) -> define symbol (ObjVar t)

    collectStmt (funcStmt body)

    modify $ \s -> s { curRetty = oldRetty }
    --collectDefault (funcRetty body) Void
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }





collectStmt :: Stmt -> DoM CollectState ()
collectStmt statement = withPos statement $ case statement of
    EmbedC _ _ -> return ()

    Block stmts -> mapM_ collectStmt stmts

    SetOp _ op expr1 expr2 -> do
        collect $ ConsEq (typeof expr1) (typeof expr2) 
        collectExpr expr1
        collectExpr expr2

    Return _ mexpr -> do
        curRetty <- gets curRetty
        collect $ ConsEq (maybe Void typeof mexpr) curRetty
        void $ traverse collectExpr mexpr

    ExprStmt expr -> do
        collect $ ConsEq (typeof expr) Void
        collectExpr expr

    Let _ pattern mexpr mstmt  -> do
        when (isJust mexpr) $ collect $ ConsEq (typeof $ fromJust mexpr) (typeof pattern)
        collectPattern pattern
        void $ traverse collectExpr mexpr
        void $ traverse collectStmt mstmt
        
    If _ expr blk melse -> do
        collect $ ConsBase Type.Bool (typeof expr)
        collectExpr expr
        collectStmt blk
        void $ traverse collectStmt melse

    Switch _ expr cases -> do
        forM_ cases $ \(pat, blk) -> do
            collect $ ConsEq (typeof pat) (typeof expr)
            collectStmt blk
            collectPattern pat
        collectExpr expr

    Data _ symbol typ mexpr -> do
        define symbol (ObjVar typ)
        void $ traverse (collectEq typ . typeof) mexpr
        void $ traverse collectExpr mexpr

    x -> error (show x)

collectPattern :: Pattern -> DoM CollectState ()
collectPattern (PatAnnotated pattern patType) = withPos pattern $ case pattern of
    PatIgnore _           -> return ()
    PatIdent _ symbol     -> define symbol (ObjVar patType)
    PatLiteral expr       -> do
        collectEq patType (typeof expr)
        collectExpr expr
    PatGuarded _ pat expr -> do
        collect $ ConsBase Type.Bool (typeof expr)
        collectPattern pat
        collectExpr expr

    PatTuple _ pats -> do
        collectDefault patType (Type.Tuple $ map typeof pats)
        collect $ ConsTuple patType (map typeof pats)
        mapM_ collectPattern pats

    PatAnnotated pat t -> do
        collectEq t patType
        collectEq t (typeof pat)
        collectPattern pat

    PatField _ symbol pats -> do
        ast <- gets astResolved
        candidates <- fmap catMaybes $ forM (Map.toList $ ctorDefs ast) $ \(symb, _) -> do
            case symbolsCouldMatch symb symbol of
                True -> return (Just symb)
                False -> return Nothing
        symbol' <- case candidates of
            [s] -> return s
            xs  -> error $ "PatField candidates for: " ++ show symbol ++ " " ++ show xs

        (s, i) <- mapGet symbol' . ctorDefs =<< gets astResolved
        collect $ ConsAdtField patType i (map typeof pats)
        mapM_ collectPattern pats

    x -> error (show x)


collectExpr :: Expr -> DoM CollectState ()
collectExpr (AExpr exprType expression) = withPos expression $ case expression of
    Prefix _ op expr -> do
        collect $ ConsEq exprType (typeof expr)
        collectExpr expr

    Float _ _ -> do
        collectDefault exprType F64

    AST.Bool _ _ -> do
        collectDefault exprType Type.Bool
        collect $ ConsBase exprType Type.Bool

    Call _ Nothing symbol exprs -> do
        collectCall exprType symbol exprs
        mapM_ collectExpr exprs

    Match _ expr pat -> do
        collect $ ConsEq (typeof pat) (typeof expr)
        collectDefault exprType Type.Bool
        collectExpr expr
        collectPattern pat

    Field _ expr symbol -> do
        collect $ ConsField (typeof expr) symbol exprType
        collectExpr expr

    AST.Reference _ expr -> do
        collect $ ConsBase exprType $ Type.Reference (typeof expr)
        collect $ ConsReference exprType (typeof expr)
        collectDefault exprType $ Type.Reference (typeof expr)
        collectExpr expr

    Dereference _ expr -> do
        collect $ ConsBase (typeof expr) (Type.Reference exprType)
        collect $ ConsReference (typeof expr) exprType
        collectExpr expr

    AST.Tuple _ exprs -> do
        collect $ ConsTuple exprType (map typeof exprs)
        collectDefault exprType $ Type.Tuple (map typeof exprs)
        mapM_ collectExpr exprs

    Builtin _ sym exprs -> do 
        case sym of
            "conv"  -> return ()
            "assert" -> do
                check (length exprs == 2) "invalid assert exprs"
                collect $ ConsBase (typeof $ exprs !! 0) Type.Bool
                collect $ ConsBase (typeof $ exprs !! 1) Type.String
                collectEq exprType Void
            "builtin_len"   -> do
                collect (ConsBase exprType I64)
                collectDefault exprType I64
            "builtin_at" -> do
                check (length exprs == 2) "invalid builtin_at call"
                collect $ ConsBase (typeof $ exprs !! 1) I64
            "builtin_table_append" -> do
                check (length exprs == 1) "invalid builtin_table_append call"
                collectEq exprType Void
            "print" -> collectEq exprType Void

        mapM_ collectExpr exprs

    Ident _ symbol -> do
        ObjVar typ <- look symbol 
        collect $ ConsIdent typ exprType

    Infix _ op expr1 expr2 -> do
        collect $ ConsEq (typeof expr1) (typeof expr2)
        case op of
            _ | op `elem` [Plus, Minus, Times, Divide, Modulo] -> do
                collect $ ConsEq exprType (typeof expr1)
            _ | op `elem` [AST.LT, AST.GT, AST.LTEq, AST.GTEq, AST.EqEq, AST.NotEq]  -> do
                collect (ConsBase exprType Type.Bool)
                collectDefault exprType Type.Bool
            _ | op `elem` [AndAnd, OrOr] -> do
                collect (ConsBase exprType Type.Bool)
                collectEq exprType (typeof expr1)
            _ -> return ()

        collectExpr expr1
        collectExpr expr2

    Int _ n -> do
        collectDefault exprType I64

    AST.String _ s -> do
        collectDefault exprType Type.String
        collect $ ConsBase exprType Type.String

    x -> error (show x)
