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


type SymTab = SymTab.SymTab Symbol () Type

data CollectState
    = CollectState
        { symTab      :: SymTab
        , curRetty    :: Type
        , collected   :: Map.Map Constraint ConstraintInfo
        , defaults    :: Map.Map Constraint ConstraintInfo
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


collect :: String -> Constraint -> DoM CollectState ()
collect msg constraint = do
    curPos <- gets curPos
    let info = ConstraintInfo { infoTextPos = curPos, infoMsg = msg }
    modify $ \s -> s { collected = Map.insert (constraint) info (collected s) }


collectDefault :: Type -> Type -> DoM CollectState ()
collectDefault t1 t2 = do
    curPos <- gets curPos
    let info = ConstraintInfo { infoTextPos = curPos, infoMsg = "default" }
    modify $ \s -> s { defaults = Map.insert (ConsDefault t1 t2) info (defaults s) }


look :: Symbol -> DoM CollectState Type
look symbol = do
    rm <- SymTab.lookup symbol () <$> gets symTab
    unless (isJust rm) (fail $ show symbol ++ " undefined")
    return (fromJust rm)


define :: Symbol -> Type -> DoM CollectState ()
define symbol obj = do
    resm <- SymTab.lookupHead symbol () <$> gets symTab
    unless (isNothing resm) (error $ show symbol ++ " already defined")
    modify $ \s -> s { symTab = SymTab.insert symbol () obj (symTab s) }


collectAST :: Prelude.Bool -> ASTResolved -> DoM CollectState ()
collectAST verbose ast = do
    --when verbose $ liftIO $ putStrLn "collecting..."
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) ->
        when (funcGenerics body == []) $
            collectFuncDef body


collectFuncDef :: FuncBody -> DoM CollectState ()
collectFuncDef body = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }
    oldRetty <- gets curRetty
    modify $ \s -> s { curRetty = typeof (funcRetty body) }
    forM_ (funcArgs body) $ \param -> case param of
        (Param _ symbol t) -> define symbol t
        (RefParam _ symbol t) -> define symbol t

    collectStmt (funcStmt body)
    modify $ \s -> s { curRetty = oldRetty }
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


collectStmt :: Stmt -> DoM CollectState ()
collectStmt statement = collectPos statement $ case statement of
    EmbedC _ _ -> return ()

    Block stmts -> mapM_ collectStmt stmts

    Return _ mexpr -> do
        curRetty <- gets curRetty
        collect "return type must match function return type" $
            ConsEq (maybe Void typeof mexpr) curRetty
        void $ traverse collectExpr mexpr

    ExprStmt expr -> do
        --collect $ ConsEq (typeof expr) Void
        collectExpr expr

    Let _ pattern Nothing mstmt  -> do
        collectPatternIsolated pattern
        void $ traverse collectStmt mstmt

    Let _ pattern mexpr mstmt  -> do
        when (isJust mexpr) $ collect "let pattern type must match expression type" $
            ConsEq (typeof $ fromJust mexpr) (typeof pattern)
        collectPattern pattern
        void $ traverse collectExpr mexpr
        void $ traverse collectStmt mstmt
        
    If _ expr blk melse -> do
        collect "if condition must have bool type" $ ConsEq Type.Bool (typeof expr)
        collectExpr expr
        collectStmt blk
        void $ traverse collectStmt melse

    For _ expr mpat blk -> do
        when (isJust mpat) $ do
            collect "for pattern type must match expression type" $
                ConsForExpr (typeof expr) (typeof $ fromJust mpat)
            collectPattern (fromJust mpat)

        collectExpr expr
        collectStmt blk

    While _ expr blk -> do
        collect "while condition must have bool type" $ ConsEq Type.Bool (typeof expr)
        collectDefault Type.Bool (typeof expr)
        collectStmt blk
        collectExpr expr

    Switch _ expr cases -> do
        forM_ cases $ \(pat, blk) -> do
            collect "switch case pattern type must match switch expression type" $
                ConsEq (typeof pat) (typeof expr)
            collectPattern pat
            collectStmt blk
        collectExpr expr

    Data _ symbol typ mexpr -> do
        define symbol typ
        void $ traverse (collect "data type must match expression type" . ConsEq typ . typeof) mexpr
        void $ traverse collectExpr mexpr

    x -> error (show x)


collectPatternIsolated :: Pattern -> DoM CollectState ()
collectPatternIsolated (PatAnnotated pattern patType) = collectPos pattern $ case pattern of
    PatIdent _ symbol -> do
        define symbol patType
    x -> error (show x)


collectPattern :: Pattern -> DoM CollectState ()
collectPattern (PatAnnotated pattern patType) = collectPos pattern $ case pattern of
    PatIgnore _           -> return ()
    PatIdent _ symbol     -> do
        define symbol patType
        collect "ident pattern must have set function" $
            ConsCall Void (Sym "set") [patType, patType]

    PatLiteral expr       -> do
        collect "expression type must match pattern type" $ ConsEq patType (typeof expr)
        collect "literal pattern needs equal function" $
            ConsCall Type.Bool (Sym "equal") [patType, patType]
        collectExpr expr

    PatGuarded _ pat expr -> do
        collect "guard expression type must have Bool type" $ ConsEq Type.Bool (typeof expr)
        collect "guard pattern type must match pattern type" $ ConsEq patType (typeof pat)
        collectPattern pat
        collectExpr expr

    PatTuple _ pats -> do
        when (length pats > 0) $ collect "first"  $ ConsCall (typeof $ pats !! 0) (Sym "first") [patType]
        when (length pats > 1) $ collect "second" $ ConsCall (typeof $ pats !! 1) (Sym "second") [patType]
        when (length pats > 2) $ collect "third"  $ ConsCall (typeof $ pats !! 2) (Sym "third") [patType]
        when (length pats > 3) $ collect "fourth" $ ConsCall (typeof $ pats !! 3) (Sym "fourth") [patType]

        mapM_ collectPattern pats

    PatAnnotated pat t -> do
        collect "pattern type must match annotation type" $ ConsEq t patType
        collect "pattern type must match annotation type" $ ConsEq t (typeof pat)
        collectPattern pat

    PatTypeField _ typ pats -> do
        collect "field pattern must be valid member of sum type" $
            ConsPatTypeField patType typ (map typeof pats)
        mapM_ collectPattern pats

    PatField _ symbol pat -> do
        collect "field pattern must be valid for sum type" $
            ConsPatField patType symbol (typeof pat)
        collectPattern pat

    PatSlice _ pats -> do
        when (length pats > 0) $ do
            collect "slice pattern must have at function" $
                ConsCall (typeof $ head pats) (Sym "at") [patType, I64]
            forM_ pats $ \pat -> do
                collect "slice pattern must all have same type" $
                    ConsEq (typeof pat) (typeof $ head pats)

        collect "slice pattern must have len function" $
            ConsCall I64 (Sym "len") [patType]
        mapM_ collectPattern pats


    x -> error (show x)


collectExpr :: Expr -> DoM CollectState ()
collectExpr (AExpr exprType expression) = collectPos expression $ case expression of
    Float _ _    -> collect "float is F64" (ConsEq exprType F64)
    AST.Bool _ _ -> collect "bool literal must have Bool type" (ConsEq exprType Type.Bool)
    Int _ _      -> collect "integer is type I64" (ConsEq exprType I64)
    AST.Char _ _ -> collect "char literal must have Char type" (ConsEq exprType Type.Char)


    Call _ symbol exprs -> do
        when (Symbol.sym symbol == "construct" && length exprs > 1) $ do
            void $ collectDefault exprType $ Type.TypeApply (Sym "Tuple") (map typeof exprs)
        when (Symbol.sym symbol == "construct" && length exprs == 1) $ do
            void $ collectDefault exprType $ typeof (head exprs)

        collect "call expression must have valid types" $
            ConsCall exprType symbol (map typeof exprs)
        mapM_ collectExpr exprs

    Match _ expr pat -> do
        collect "match must have same type for pattern and expression" $
            ConsEq (typeof pat) (typeof expr)
        collectDefault exprType Type.Bool
        collectExpr expr
        collectPattern pat

    Field _ expr idx -> do
        collect "field access must have valid types" $
            ConsField (typeof expr) idx exprType
        collectExpr expr

    AST.Reference _ expr -> do
        collect "reference type must match expression type" $ ConsEq exprType (typeof expr)
        collectExpr expr

    Builtin _ sym exprs -> do 
        case sym of
            "builtin_table_at" -> do
                check (length exprs == 2) "invalid builtin_table_at call"
                collect "builtin must have I64 type for index argument" $
                    ConsEq (typeof $ exprs !! 1) I64
                collect "builtin argument must have table base type" $
                    ConsBase (typeof $ exprs !! 0) (Type.TypeApply (Sym "Table") [exprType])

            "builtin_array_at" -> do
                check (length exprs == 2) "invalid builtin_table_at call"
                collect "builtin must have I64 type for index" $
                    ConsEq (typeof $ exprs !! 1) I64

            "builtin_slice_at" -> do
                check (length exprs == 2) "invalid builtin_table_at call"
                collect "builtin must have I64 type for index" $
                    ConsEq (typeof $ exprs !! 1) I64
                collect "builtin must have slice argument" $
                    ConsEq (typeof $ exprs !! 0) (Type.Slice exprType)
                

            "builtin_table_append" -> do
                check (length exprs == 1) "invalid builtin_table_append call"
                collect "builtin returns void" $ ConsEq exprType Void

            "builtin_table_slice" -> do
                check (length exprs == 3) "invalid builtin_table_slice call"
                collect "builtin must have slice argument" $
                    ConsSlice exprType (typeof $ head exprs)

            "conv"  -> return ()
            "assert" -> do
                check (length exprs == 2) "invalid assert exprs"
                collect "assert must have Bool argument" $
                    ConsEq (typeof $ exprs !! 0) Type.Bool
                collect "assert must have Char.Slice message" $
                    ConsEq (typeof $ exprs !! 1) (Type.Slice Type.Char)
                collect "assert returns void" $
                    ConsEq exprType Void

        mapM_ collectExpr exprs

    Ident _ symbol -> do
        typ <- look symbol 
        collect ("identifier type for " ++ show symbol ++ " must match expression type") $
            ConsEq typ exprType


    AST.String _ s -> do
        collect "string literal must have Char.Slice type" $ ConsEq exprType (Type.Slice Type.Char)

    AST.Array _ exprs -> do
        when (length exprs > 0) $ do
            forM_ (zip exprs [0..]) $ \(expr, i) ->
                collect "elements in array must have same type" $
                    ConsEq (typeof $ exprs !! i) (typeof $ head exprs)
            collect "array expression must have slice type" $
                ConsEq exprType (Type.Slice $ typeof $ head exprs)
        mapM_ collectExpr exprs



    x -> error (show x)
