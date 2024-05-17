{-# LANGUAGE FlexibleInstances #-}
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
import FeatureFinder


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


instance TypeDefs (DoM CollectState) where
    getTypeDefs = gets (typeDefsAll . astResolved)


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


collectCall :: Symbol -> Type -> DoM CollectState ()
collectCall symbol callType | typeFullyResolved callType = return ()
collectCall symbol callType = do
    headers <- gets $ filter (\x -> symbolsCouldMatch (funcSymbol x) symbol)
        . Map.elems . Map.map funcHeader . funcDefsAll . astResolved
    candidates <- map (flatten . typeof) <$> findCandidates callType headers
    -- TODO needs to check only visible symbols

    --ast <- gets astResolved
    --_ <- runDoMExcept ast $ getFeatureArgFromFuncCall (CallHeader symbol argTypes retType)

    forM_ (zip [0..] (flatten callType)) $ \(i, t) -> do
        case allSameType (map (!! i) candidates) of
            Just x | typeFullyResolved x -> collect "call" (ConsEq t x)
            _ -> return ()
    where
        flatten :: Type -> [Type]
        flatten (Apply Type.Func ts) = ts

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

    Call _ callType exprs -> do
        Apply Type.Func ts <- baseTypeOf callType
        unless ((length exprs + 1) == length ts) (fail "invalid function type arguments")
        collect "call return" $ ConsEq exprType (head ts)
        forM_ (zip (tail ts) exprs) $ \(t, expr) -> do
            collect "call arg" $ ConsEq (typeof expr) t
        mapM_ collectExpr exprs


    Match _ expr pat -> do
        collect "match must have same type for pattern and expression" $
            ConsEq (typeof pat) (typeof expr)
        collect "match type is Bool" $ ConsEq exprType Type.Bool
        collectExpr expr
        collectPattern pat

    Field _ expr idx -> do
        collect "field access must have valid types" $
            ConsField (typeof expr) idx exprType
        collectExpr expr

    AST.Reference _ expr -> do
        collect "reference type must match expression type" $ ConsEq exprType (typeof expr)
        collectExpr expr

    Ident _ symbol -> do
        typ <- look symbol 
        collect ("identifier type for " ++ prettySymbol symbol ++ " must match expression type") $
            ConsEq typ exprType

    AST.String _ s -> do
        collect "string literal must have Char.Slice type" $ ConsEq exprType (Apply Type.Slice [Type.Char])

    AST.Array _ exprs -> do
        when (length exprs > 0) $ do
            forM_ (zip exprs [0..]) $ \(expr, i) ->
                collect "elements in array must have same type" $
                    ConsEq (typeof $ exprs !! i) (typeof $ head exprs)
            collect "array expression must have slice type" $
                ConsEq exprType (Apply Type.Slice [typeof $ head exprs])
        mapM_ collectExpr exprs

    x -> error (show x)
