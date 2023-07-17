{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module COptimise where

import qualified Data.Map as Map
import Control.Monad.State

import Monad
import CAst
import CBuilder


modifyElem :: BoM BuilderState m => ID -> (Element -> m Element) -> m ()
modifyElem id f = do
    elem <- (Map.! id) <$> gets elements
    elem' <- f elem
    modify $ \s -> s { elements = Map.insert id elem' (elements s) }



optimise :: BoM BuilderState m => m ()
optimise = do
    liftIO $ putStrLn "running optimisation pass..."
    elems <- Map.toList <$> gets elements

    forM_ elems $ \(id, elem) -> case elem of
        -- empty switch
        Switch expr [] -> do modifyElem id $ \_ -> return $ ExprStmt expr
        Assign a b expr -> do modifyElem id $ \_ -> Assign a b <$> optimiseExpr expr

        If expr stmts -> do
            expr' <- optimiseExpr expr
            stmts' <- optimiseStmts stmts
            modifyElem id $ \_ -> return $ If expr' stmts'

        For mpre mcnd mpost stmts -> do
            mpre' <- maybe (return Nothing) (fmap Just . optimiseExpr) mpre
            stmts' <- optimiseStmts stmts
            modifyElem id $ \_ -> return $ For mpre' mcnd mpost stmts'

        Case expr stmts -> do
            expr' <- optimiseExpr expr
            stmts' <- optimiseStmts stmts
            modifyElem id $ \_ -> return $ Case expr' stmts'
        
        func@(Func _ _ _ _) -> do
            stmts' <- optimiseStmts (funcBody func)
            modifyElem id $ \_ -> return $ func { funcBody = stmts' }

        _ -> return ()
        
    return ()


optimiseStmts :: BoM BuilderState m => [ID] -> m [ID]
optimiseStmts stmts = optimiseStmtPairs =<< optimiseStmtSingles stmts


optimiseStmtSingles :: BoM BuilderState m => [ID] -> m [ID]
optimiseStmtSingles [] = return []
optimiseStmtSingles (id:xs) = do
    elem <- (Map.! id) <$> gets elements
    case elem of
        -- useless assert
        ExprStmt (Call "assert" [Bool True]) -> optimiseStmtSingles xs

        ExprStmt e | exprNoEffects e -> optimiseStmtSingles xs

        _ -> (id:) <$> optimiseStmtSingles xs


optimiseStmtPairs :: BoM BuilderState m => [ID] -> m [ID]
optimiseStmtPairs [] = return []
optimiseStmtPairs [x] = return [x]
optimiseStmtPairs (x:y:xs) = do
    ex <- (Map.! x) <$> gets elements
    ey <- (Map.! y) <$> gets elements
    case (ex, ey) of
        (If _ _, Else _)               -> (x:) <$> optimiseStmtPairs (y:xs)
        (If (Bool False) _, _)         -> optimiseStmtPairs (y:xs)

        (If e [], _) | exprNoEffects e           -> optimiseStmtPairs (y:xs)
        (Return _, elem) | elemDeleteable elem   -> optimiseStmtPairs (x:xs)
        (ReturnVoid, elem) | elemDeleteable elem -> optimiseStmtPairs (x:xs)
        (Break, elem) | elemDeleteable elem      -> optimiseStmtPairs (x:xs)

        _ -> (x:) <$> optimiseStmtPairs (y:xs)



optimiseExpr :: BoM BuilderState m => Expression -> m Expression
optimiseExpr expr = case expr of
    Not (Bool b)               -> return (Bool $ not b)
    Infix AndAnd (Bool True) e -> return e
    Infix AndAnd e (Bool True) -> return e
    _ -> return expr


elemDeleteable :: Element -> Bool
elemDeleteable elem = case elem of
    Label _      -> False
    Break        -> True
    (ExprStmt _) -> True
    _ -> True


exprNoEffects :: Expression -> Bool
exprNoEffects expr = case expr of
    Ident _ -> True
    Member e _ -> exprNoEffects e
    Subscript e1 e2 -> exprNoEffects e1 && exprNoEffects e2
    Bool _ -> True
    Int n -> True
    Char c -> True
    String s -> True
    _ -> False
