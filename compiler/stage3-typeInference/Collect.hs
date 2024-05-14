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
import Symbol
import ASTResolved
import FunctionFinder


data CollectState
    = CollectState
        { symTab      :: Map.Map Symbol Type
        , curRetty    :: Type
        , collected   :: Map.Map Constraint ConstraintInfo
        , defaults    :: Map.Map Constraint ConstraintInfo
        , curPos      :: TextPos
        , astResolved :: ASTResolved
        }

initCollectState ast = CollectState
    { symTab      = Map.empty
    , curRetty    = Void
    , collected   = Map.empty
    , defaults    = Map.empty
    , curPos      = TextPos "" 0 0
    , astResolved = ast
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
    --liftIO $ putStrLn $ "looking: " ++ prettySymbol symbol
    rm <- Map.lookup symbol <$> gets symTab
    unless (isJust rm) (fail $ prettySymbol symbol ++ " undefined")
    return (fromJust rm)


define :: Symbol -> Type -> DoM CollectState ()
define symbol obj = do
    --liftIO $ putStrLn $ "defining: " ++ prettySymbol symbol
    resm <- Map.lookup symbol <$> gets symTab
    unless (isNothing resm) (error $ prettySymbol symbol ++ " already defined")
    modify $ \s -> s { symTab = Map.insert symbol obj (symTab s) }


collectFuncDef :: Func -> DoM CollectState ()
collectFuncDef func = do
    oldRetty <- gets curRetty
    modify $ \s -> s { curRetty = typeof (funcRetty (funcHeader func)) }
    forM_ (funcArgs $ funcHeader func) $ \param -> case param of
        (Param _ symbol t) -> define symbol t
        (RefParam _ symbol t) -> define symbol t

    collectStmt (funcStmt func)
    modify $ \s -> s { curRetty = oldRetty }


collectStmt :: Stmt -> DoM CollectState ()
collectStmt statement = collectPos statement $ case statement of
    EmbedC _ _ -> return ()

    Block stmts -> mapM_ collectStmt stmts

    Return _ mexpr -> do
        curRetty <- gets curRetty
        when (isJust mexpr && curRetty == Void) (fail "cannot return in void function")
        collect "return type must match function return type" $
            ConsEq (maybe Void typeof mexpr) curRetty
        void $ traverse collectExpr mexpr

    ExprStmt expr -> collectExpr expr

    Let _ pattern Nothing Nothing  -> do
        collectPattern pattern
        
    If _ expr blk melse -> do
        collect "if condition must have bool type" $ ConsEq Type.Bool (typeof expr)
        collectExpr expr
        collectStmt blk
        void $ traverse collectStmt melse

    While _ expr blk -> do
        collect "while condition must have bool type" $ ConsEq Type.Bool (typeof expr)
        collectDefault Type.Bool (typeof expr)
        collectExpr expr
        collectStmt blk

    Data _ symbol typ mexpr -> do
        define symbol typ
        void $ traverse (collect "data type must match expression type" . ConsEq typ . typeof) mexpr
        void $ traverse collectExpr mexpr

    Assign pos symbol expr -> do
        define symbol (typeof expr)
        collectExpr expr

    x -> error "invalid statement"


collectCall :: Symbol -> [Type] -> Type -> DoM CollectState ()
collectCall symbol argTypes retType
    | all typeFullyResolved (retType : argTypes) = return ()
collectCall symbol argTypes retType = do
    headers <- gets (Map.elems . Map.map funcHeader . funcDefsAll . astResolved)
    candidates <- findCandidates (CallHeader symbol argTypes retType) headers
    -- TODO needs to check only visible symbols

    case allSameType (map (typeof . AST.funcRetty) candidates) of
        Just x | typeFullyResolved x -> collect "call" $ ConsEq retType x
        _ -> return ()

    forM_ (zip [0..] argTypes) $ \(i, at) -> do
        case allSameType (map (typeof . (!! i) . AST.funcArgs) candidates) of
            Just x | typeFullyResolved x -> collect "call" (ConsEq at x)
            _ -> return ()
    where
        allSameType :: [Type] -> Maybe Type
        allSameType [] = Nothing
        allSameType [x] = Just x
        allSameType (x:xs) = case allSameType xs of
            Just t -> if x == t then Just t else Nothing
            Nothing -> Nothing


collectPattern :: Pattern -> DoM CollectState ()
collectPattern (PatAnnotated pattern patType) = collectPos pattern $ case pattern of
    PatIdent _ symbol     -> do
        define symbol patType

    x -> error (show x)


collectExpr :: Expr -> DoM CollectState ()
collectExpr (AExpr exprType expression) = collectPos expression $ case expression of
    Float _ _    -> collect "float is F64" (ConsEq exprType F64)
    AST.Bool _ _ -> collect "bool literal must have Bool type" (ConsEq exprType Type.Bool)
    Int _ _      -> collect "integer is type I64" (ConsEq exprType I64)
    AST.Char _ _ -> collect "char literal must have Char type" (ConsEq exprType Type.Char)

    Call _ symbol exprs -> do
        when (symbolsCouldMatch symbol (Sym ["Construct", "construct"]) && length exprs > 1) $ do
            void $ collectDefault exprType $ Apply (TypeDef $ Sym ["Tuple"]) (map typeof exprs)
   
        when (symbolsCouldMatch symbol (Sym ["Construct", "construct"]) && length exprs == 1) $ do
            void $ collectDefault exprType $ typeof (head exprs)

        collectCall symbol (map typeof exprs) exprType
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
        case (sym, exprs) of
            ("builtin_table_at", [tab, idx]) -> do
                collect "index must be I64" $ ConsEq (typeof idx) I64
                collect "invalid builtin" $ ConsEq (typeof tab) $
                    Apply (TypeDef $ Sym ["Table"]) [exprType]

            ("builtin_array_at", [arr, idx]) -> do
                collect "index must be I64" $ ConsEq (typeof idx) I64

            ("builtin_slice_at", [slc, idx]) -> do
                collect "index must be I64" $ ConsEq (typeof idx) I64
                collect "invalid slice type" $ ConsEq (typeof slc) (Slice exprType)
                
            ("builtin_table_append", [tab]) -> do
                collect "void function" (ConsEq exprType Void)

            ("builtin_table_slice", [tab, start, end]) -> do
                collect "invalid slice type" $ ConsSlice exprType (typeof tab)
                collect "start must be I64" $ ConsEq (typeof start) I64
                collect "end must be I64" $ ConsEq (typeof end) I64

            ("builtin_sum_enum", [sum]) -> do
                collect "function returns I64" $ ConsEq exprType I64

            ("builtin_sum_reset", [sum, en]) -> do
                collect "enum value must be I64" $ ConsEq (typeof en) I64
                collect "function returns void" $ ConsEq exprType Void

            ("builtin_zero", []) -> return ()
            ("builtin_pretend", [_]) -> return ()

            ("builtin_store", [dst, src]) -> do
                collect "builtin_store arguments must have same type" $ ConsEq (typeof dst) (typeof src)
                collect "builtin_store returns void" $ ConsEq Void exprType

            ("builtin_add", [a, b]) -> do
                collect "builtin_add arguments must have same type" $ ConsEq (typeof a) (typeof b)
                collect "builtin_add returns same type as arguments" $ ConsEq exprType (typeof a)

            ("builtin_subtract", [a, b]) -> do
                collect "builtin_subtract arguments must have same type" $ ConsEq (typeof a) (typeof b)
                collect "builtin_subtract returns same type as arguments" $ ConsEq exprType (typeof a)

            ("builtin_multiply", [a, b]) -> do
                collect "builtin_multiply arguments must have same type" $ ConsEq (typeof a) (typeof b)
                collect "builtin_multiply returns same type as arguments" $ ConsEq exprType (typeof a)

            ("builtin_divide", [a, b]) -> do
                collect "builtin_divide arguments must have same type" $ ConsEq (typeof a) (typeof b)
                collect "builtin_divide returns same type as arguments" $ ConsEq exprType (typeof a)

            ("builtin_modulo", [a, b]) -> do
                collect "builtin_modulo arguments must have same type" $ ConsEq (typeof a) (typeof b)
                collect "builtin_modulo returns same type as arguments" $ ConsEq exprType (typeof a)

            ("builtin_equal", [a, b]) -> do
                collect "builtin_equal arguments must have same type" $ ConsEq (typeof a) (typeof b)
                collect "builtin_equal returns Bool" $ ConsEq exprType Type.Bool

            ("builtin_len", [x]) -> do
                collect "builtin_len returns I64" $ ConsEq exprType Type.I64

            _ -> fail "invalid builtin arguments"

        mapM_ collectExpr exprs

    Ident _ symbol -> do
        typ <- look symbol 
        collect ("identifier type for " ++ prettySymbol symbol ++ " must match expression type") $
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
