{-# LANGUAGE FlexibleInstances #-}
module Collect where

import Data.Maybe
import qualified Data.Map as Map

import AST
import Type
import Constraint
import Monad
import Error
import Control.Monad.State
import Symbol
import ASTResolved


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


collectStmt :: Stmt -> DoM CollectState ()
collectStmt statement = collectPos statement $ case statement of
    FuncDef generics (AST.Func header stmt) -> do
        oldRetty <- gets curRetty
        modify $ \s -> s { curRetty = typeof (funcRetty header) }
        forM_ (funcArgs header) $ \param -> 
            define (paramSymbol param) (typeof param)
        collectStmt stmt
        modify $ \s -> s { curRetty = oldRetty }


    Aquires pos generics typ args isRef stmt -> do
        (Type.Func, retty : argTypes) <- unfoldType <$> baseTypeOf typ

        unless (length argTypes == length args) (fail "arg length mismatch")

        oldRetty <- gets curRetty
        modify $ \s -> s { curRetty = retty }

        forM_ (zip args argTypes) $ \(arg, t) -> case arg of
            Param _ symbol _    -> define symbol t
            RefParam _ symbol _ -> define symbol t

        collectStmt stmt

        modify $ \s -> s { curRetty = oldRetty }

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
        -- at{ t0, t1, t2 }                     => Func{ t2, t0.t1, t2 }
        -- at( x : Array{3, I64}, idx : t3 ):t4 -> Func{ t4, Array.3.I64, t3 }

        -- len{ t0 }( x:t1 ):t2 => Func{ I64, t0 }
        --          ( x:t1 ):t2 => Func{ t2 , t1 }


        (Type.Func, retType : argTypes) <- unfoldType <$> baseTypeOf callType
        unless (length exprs == length argTypes)
            (fail $ "invalid function type arguments: " ++ show callType)

        collect "call return" (ConsEq exprType retType)
        zipWithM (\x y -> collect "call argument" (ConsEq x y)) argTypes (map typeof exprs)

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
        collect "string literal must have Char.Slice type" $ ConsEq exprType (Apply Type.Slice Type.Char)

    AST.Array _ exprs -> do
        when (length exprs > 0) $ do
            forM_ (zip exprs [0..]) $ \(expr, i) ->
                collect "elements in array must have same type" $
                    ConsEq (typeof $ exprs !! i) (typeof $ head exprs)
            collect "array expression must have slice type" $
                ConsEq exprType $ Apply Type.Slice (typeof $ head exprs)
        mapM_ collectExpr exprs

    x -> error (show x)
