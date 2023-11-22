module COptimise where

import qualified Data.Map as Map
import Control.Monad.State

import Monad
import CAst
import CBuilder
import Error


modifyElem :: ID -> (Element -> DoM BuilderState Element) -> DoM BuilderState ()
modifyElem id f = do
    elem <- (Map.! id) <$> gets elements
    elem' <- f elem
    modify $ \s -> s { elements = Map.insert id elem' (elements s) }


optimise :: DoM BuilderState ()
optimise = do
    elems <- Map.toList <$> gets elements
    forM_ elems $ \(id, elem) -> case elem of
        Switch expr []  -> modifyElem id $ \_ -> return $ ExprStmt expr
        Assign a b expr -> modifyElem id $ \_ -> return $ Assign a b (optimiseExpr expr)
        ExprStmt expr   -> modifyElem id $ \_ -> return $ ExprStmt (optimiseExpr expr)

        If expr stmts -> do
            expr' <- return $ optimiseExpr expr
            stmts' <- optimiseStmts stmts
            modifyElem id $ \_ -> return $ If expr' stmts'

        For mpre mcnd mpost stmts -> do
            mpre' <- return $ fmap optimiseExpr mpre
            stmts' <- optimiseStmts stmts
            modifyElem id $ \_ -> return $ For mpre' mcnd mpost stmts'

        Case expr stmts -> do
            expr' <- return $ optimiseExpr expr
            stmts' <- optimiseStmts stmts
            modifyElem id $ \_ -> return $ Case expr' stmts'
        
        func@(Func _ _ _ _) -> do
            stmts' <- optimiseStmts (funcBody func)
            modifyElem id $ \_ -> return $ func { funcBody = stmts' }

        _ -> return ()


optimiseStmts :: [ID] -> DoM BuilderState [ID]
optimiseStmts stmts = optimiseStmtPairs =<< optimiseStmtSingles stmts


optimiseStmtSingles :: [ID] -> DoM BuilderState [ID]
optimiseStmtSingles [] = return []
optimiseStmtSingles (id:xs) = do
    elem <- mapGet id =<< gets elements
    case elem of
        ExprStmt (Call "assert" [Bool True]) -> optimiseStmtSingles xs
        ExprStmt e | exprNoEffects e -> optimiseStmtSingles xs

        _ -> (id:) <$> optimiseStmtSingles xs


optimiseStmtPairs :: [ID] -> DoM BuilderState [ID]
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


optimiseExpr :: Expression -> Expression
optimiseExpr expr = case expr of
    Not (Bool b)               -> (Bool $ not b)
    Not (Not e)                -> optimiseExpr e
    Not (Infix EqEq e1 e2)     -> optimiseExpr (Infix NotEq e1 e2)
    Infix AndAnd (Bool True) e -> optimiseExpr e
    Infix AndAnd e (Bool True) -> optimiseExpr e
    Infix EqEq e (Bool True)   -> optimiseExpr e
    Infix EqEq (Bool True) e   -> optimiseExpr e
    Infix EqEq e (Bool False)  -> optimiseExpr (Not e)
    Infix EqEq (Bool False) e  -> optimiseExpr (Not e)

    Infix NotEq e (Bool True)  -> optimiseExpr (Not e)
    Infix NotEq (Bool True) e  -> optimiseExpr (Not e)
    Infix NotEq e (Bool False) -> optimiseExpr e
    Infix NotEq (Bool False) e -> optimiseExpr e
    Address (Deref e)          -> optimiseExpr e
    Deref (Address e)          -> optimiseExpr e
    Member (Deref e) str       -> PMember (optimiseExpr e) str
    PMember (Address e) str    -> Member (optimiseExpr e) str

    Not e                      -> Not (optimiseExpr e)
    Infix op e1 e2             -> Infix op (optimiseExpr e1) (optimiseExpr e2)
    Call symbol args           -> Call symbol (map optimiseExpr args)
    Address e                  -> Address (optimiseExpr e)
    Deref e                    -> Deref (optimiseExpr e)
    Initialiser es             -> Initialiser (map optimiseExpr es)
    Subscript e1 e2            -> Subscript (optimiseExpr e1) (optimiseExpr e2)

    _ -> expr


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
