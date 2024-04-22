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
        }

initCollectState = CollectState
    { symTab      = SymTab.initSymTab
    , curRetty    = Void
    , collected   = Map.empty
    , defaults    = Map.empty
    , curPos      = TextPos "" 0 0
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
    unless (isJust rm) (fail $ show symbol ++ " undefined")
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


collectFuncDef :: Symbol -> FuncBody -> DoM CollectState ()
collectFuncDef symbol body = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }
    oldRetty <- gets curRetty
    modify $ \s -> s { curRetty = typeof (funcRetty body) }
    forM (funcParams body) $ \(Param _ symbol t) -> error ""
    forM_ (funcArgs body) $ \param -> case param of
        (Param _ symbol t) -> define symbol (ObjVar t)
        (RefParam _ symbol t) -> define symbol (ObjVar t)

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
        --collect $ ConsEq (typeof expr) Void
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

    For _ expr mpat blk -> do
        when (isJust mpat) $ do
            collect $ ConsForExpr (typeof expr) (typeof $ fromJust mpat)
            collectPattern (fromJust mpat)

        collectExpr expr
        collectStmt blk

    While _ expr blk -> do
        collect $ ConsBase Type.Bool (typeof expr)
        collectDefault Type.Bool (typeof expr)
        collectStmt blk
        collectExpr expr

    Switch _ expr cases -> do
        forM_ cases $ \(pat, blk) -> do
            collect $ ConsEq (typeof pat) (typeof expr)
            collectPattern pat
            collectStmt blk
        collectExpr expr

    Data _ symbol typ mexpr -> do
        define symbol (ObjVar typ)
        void $ traverse (collectEq typ . typeof) mexpr
        void $ traverse collectExpr mexpr

    x -> error (show x)

collectPattern :: Pattern -> DoM CollectState ()
collectPattern (PatAnnotated pattern patType) = withPos pattern $ case pattern of
    PatIgnore _           -> return ()
    PatIdent _ symbol     -> do
        define symbol (ObjVar patType)
    PatLiteral expr       -> do
        collectEq patType (typeof expr)
        collectExpr expr
    PatGuarded _ pat expr -> do
        collect $ ConsBase Type.Bool (typeof expr)
        collect $ ConsEq patType (typeof pat)
        collectPattern pat
        collectExpr expr

    PatTuple _ pats -> do
        collectDefault patType (Type.TypeApply (Sym "Tuple") $ map typeof pats)
        collect $ ConsBase patType $ Type.TypeApply (Sym "Tuple") (map typeof pats)
        mapM_ collectPattern pats

    PatAnnotated pat t -> do
        collectEq t patType
        collectEq t (typeof pat)
        collectPattern pat

    PatTypeField _ typ pats -> do
        collect $ ConsPatTypeField patType typ (map typeof pats)
        mapM_ collectPattern pats

    PatField _ symbol pat -> do
        collect $ ConsPatField patType symbol (typeof pat)
        collectPattern pat


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

    Call _ symbol exprs -> do
        collect $ ConsCall exprType symbol (map typeof exprs)
        mapM_ collectExpr exprs

    Construct _ typ exprs -> do
        collect $ ConsEq exprType typ
        mapM_ collectExpr exprs

    Match _ expr pat -> do
        collect $ ConsEq (typeof pat) (typeof expr)
        collectDefault exprType Type.Bool
        collectExpr expr
        collectPattern pat

    Field _ expr idx -> do
        collect $ ConsField (typeof expr) idx exprType
        collectExpr expr

    AST.Reference _ expr -> do
        collect $ ConsEq exprType (typeof expr)
        collectExpr expr

    AST.Tuple _ exprs -> do
        collect $ ConsBase exprType $ Type.TypeApply (Sym "Tuple") (map typeof exprs)
        collectDefault exprType $ Type.TypeApply (Sym "Tuple") (map typeof exprs)
        mapM_ collectExpr exprs

    Builtin _ sym exprs -> do 
        case sym of
            "builtin_table_at" -> do
                check (length exprs == 2) "invalid builtin_table_at call"
                collect $ ConsBase (typeof $ exprs !! 1) I64
                collect $ ConsBase (typeof $ exprs !! 0) (Type.TypeApply (Sym "Table") [exprType])

            "builtin_table_append" -> do
                check (length exprs == 1) "invalid builtin_table_append call"
                collectEq exprType Void

            "builtin_table_slice" -> do
                check (length exprs == 1) "invalid builtin_table_slice call"
                collect $ ConsSlice exprType (typeof $ head exprs)

            "conv"  -> return ()
            "assert" -> do
                check (length exprs == 2) "invalid assert exprs"
                collect $ ConsBase (typeof $ exprs !! 0) Type.Bool
                collect $ ConsBase (typeof $ exprs !! 1) (Type.Slice Type.Char)
                collectEq exprType Void
            "builtin_len"   -> do
                collect (ConsBase exprType I64)
                collectDefault exprType I64
            "print" -> collectEq exprType Void

        mapM_ collectExpr exprs

    Ident _ symbol -> do
        ObjVar typ <- look symbol 
        collect $ ConsEq typ exprType

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

    AST.Char _ c -> do
        collect $ ConsBase exprType Type.Char
        collectDefault exprType Type.Char

    AST.String _ s -> do
        collect $ ConsEq exprType (Type.Slice Type.Char)

    x -> error (show x)
