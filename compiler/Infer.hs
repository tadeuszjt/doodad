{-# LANGUAGE FlexibleContexts #-}
module Infer where

import Control.Monad.State
import Data.Maybe
import Data.Word
import qualified Data.Map as Map


import AST
import Type
import Error
import Monad

type TypeId = Int
type ExprId = Int

data InferState =
    InferState
    { expressions  :: Map.Map ExprId (Expr, TypeId)
    , exprIdSupply :: ExprId
    , typeIdSupply :: TypeId
    , typeEqPairs  :: [(TypeId, TypeId)]
    , typeDefaults :: Map.Map TypeId Type
    , typeDefs     :: Map.Map TypeId Type
    }
    deriving (Show)

initInferState =
    InferState
    { expressions  = Map.empty
    , exprIdSupply = 0
    , typeIdSupply = 0
    , typeEqPairs  = []
    , typeDefaults = Map.empty
    , typeDefs     = Map.empty
    }


infAddExpr :: BoM InferState m => Expr -> TypeId -> m ExprId
infAddExpr expr tid = do
    id <- gets exprIdSupply
    modify $ \s -> s { expressions = Map.insert id (expr, tid) (expressions s) }
    modify $ \s -> s { exprIdSupply = id + 1 }
    return id


infGenTypeId :: BoM InferState m => m TypeId
infGenTypeId = do
    id <- gets typeIdSupply
    modify $ \s -> s { typeIdSupply = id + 1 }
    return id


infAddTypeEq :: BoM InferState m => TypeId -> TypeId -> m ()
infAddTypeEq t1 t2 = do
    modify $ \s -> s { typeEqPairs = (t1, t2) : (typeEqPairs s) }


infAddTypeToId:: BoM InferState m => TypeId -> Type -> m ()
infAddTypeToId tid typ = do
    tm <- Map.lookup tid <$> gets typeDefs
    when (isJust tm) $ fail $ show tid ++ " already has type definition"
    modify $ \s -> s { typeDefs = Map.insert tid typ (typeDefs s) }
    

infAddDefaultTypeToId :: BoM InferState m => TypeId -> Type -> m ()
infAddDefaultTypeToId tid typ =
    modify $ \s -> s { typeDefaults = Map.insert tid typ (typeDefaults s) }


infExpr :: BoM InferState m => Expr -> m ExprId
infExpr expr = case expr of
    Int p n -> do
        t <- infAddExpr expr =<< infGenTypeId
        infAddDefaultTypeToId t I64
        return t

    AST.Bool p b         -> do
        t <- infAddExpr expr =<< infGenTypeId
        infAddTypeToId t Type.Bool
        return t 

    Infix p op e1 e2
        | op == EqEq || op == OrOr || op == AndAnd -> do
            et1 <- infExpr e1
            et2 <- infExpr e2
            t <- infGenTypeId
            infAddTypeEq et1 et2
            infAddTypeToId t Type.Bool
            infAddExpr (Infix p op (Expr et1) (Expr et2)) t

    Infix p op e1 e2
        | op == Plus || op == Times -> do
            et1 <- infExpr e1
            et2 <- infExpr e2
            t <- infGenTypeId
            infAddTypeEq et1 et2
            infAddExpr (Infix p op (Expr et1) (Expr et2)) et1


infStmt :: BoM InferState m => Stmt -> m Stmt
infStmt stmt = case stmt of
    Assign pos (PatIdent p sym) expr -> do
        eid <- infExpr expr
        return $ Assign p (PatIdent p sym) (Expr eid)


infResolve :: BoM InferState m => m ()
infResolve = do
    (t0, t1) <- head <$> gets typeEqPairs
    --substitute t0 t1
    return ()

    where
        -- replaced t1 with t2
        substitute :: BoM InferState m => TypeId -> TypeId -> m ()
        substitute t1 t2 = do
            exprs <- gets expressions
            forM_ (Map.toList exprs) $ \(eid, (expr, tid)) -> do
                when (tid == t1) $ modify $ \s -> s { expressions = Map.insert eid (expr, t2) (expressions s) }

            modify $ \s -> s { typeDefaults = Map.delete t1 (typeDefaults s) }

            modify $ \s -> s { typeEqPairs = [ (if ta == t2 then t1 else ta, if tb == t2 then t1 else tb) | (ta, tb) <- (typeEqPairs s) ] }
            modify $ \s -> s { typeEqPairs = [ (ta, tb) | (ta, tb) <- (typeEqPairs s), ta /= tb ] }
        
        

prettyInferState :: InferState -> IO ()
prettyInferState state = do
    putStrLn "Expressions"

    forM_ (Map.toList $ expressions state) $ \(eid, (expr, tid)) ->
        putStrLn $ "e" ++ show eid ++ ":t" ++ show tid ++ " " ++ show expr

    putStrLn ""
    putStrLn "Types"

    forM_ (Map.toList $ typeDefs state) $ \(tid, t) ->
        putStrLn $ "t" ++ show tid ++ " " ++ show t

    forM_ (typeEqPairs state) $ \(t1, t2) ->
        putStrLn $ "t" ++ show t1 ++ " = " ++ "t" ++ show t2

    forM_ (Map.toList $ typeDefaults state) $ \(tid, typ) ->
        putStrLn $ "t" ++ show tid ++ " default: " ++ show typ
