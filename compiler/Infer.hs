{-# LANGUAGE FlexibleContexts #-}
module Infer where

import Control.Monad.State
import Data.Maybe
import Data.Word
import qualified Data.Map as Map

import qualified SymTab
import AST
import Type
import Error
import Monad

newtype TypeId = TypeId Int
    deriving (Eq, Ord)

newtype ExprId = ExprId Int
    deriving (Eq, Ord)

newtype SymId = SymId Int
    deriving (Eq, Ord)

instance Show TypeId where show (TypeId i) = 't' : show i
instance Show ExprId where show (ExprId i) = 'e' : show i
instance Show SymId where show (SymId i) = 's' : show i

data InferState =
    InferState
    { expressions  :: Map.Map ExprId (Expr, TypeId)
    , exprIdSupply :: Int
    , typeIdSupply :: Int
    , symIdSupply  :: Int
    , typeEqPairs  :: [(TypeId, TypeId)]
    , typeDefaults :: Map.Map TypeId Type
    , typeDefs     :: Map.Map TypeId Type
    , symbols      :: Map.Map SymId (String, TypeId)
    , symTab       :: SymTab.SymTab String () SymId
    }
    deriving (Show)

initInferState =
    InferState
    { expressions  = Map.empty
    , exprIdSupply = 0
    , typeIdSupply = 0
    , symIdSupply  = 0
    , typeEqPairs  = []
    , typeDefaults = Map.empty
    , typeDefs     = Map.empty
    , symbols      = Map.empty
    , symTab       = SymTab.initSymTab
    }


infAddExpr :: BoM InferState m => Expr -> TypeId -> m ExprId
infAddExpr expr tid = do
    eid <- gets exprIdSupply
    modify $ \s -> s { expressions = Map.insert (ExprId eid) (expr, tid) (expressions s) }
    modify $ \s -> s { exprIdSupply = eid + 1 }
    return (ExprId eid)


infGenTypeId :: BoM InferState m => m TypeId
infGenTypeId = do
    n <- gets typeIdSupply
    modify $ \s -> s { typeIdSupply = n + 1 }
    return (TypeId n)


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


infGetTid :: BoM InferState m => ExprId -> m TypeId
infGetTid eid = do
    (_, tid) <- (Map.! eid) <$> gets expressions
    return tid

infGetSymTid :: BoM InferState m => SymId -> m TypeId
infGetSymTid sid = do
    (_, tid) <- (Map.! sid) <$> gets symbols
    return tid

infAddSym :: BoM InferState m => String -> TypeId -> m SymId
infAddSym sym tid = do
    id <- gets symIdSupply
    modify $ \s -> s { symIdSupply = id + 1 }
    modify $ \s -> s { symbols = Map.insert (SymId id) (sym, tid) (symbols s) }
    modify $ \s -> s { symTab = SymTab.insert sym () (SymId id) (symTab s) }
    return (SymId id)


infExpr :: BoM InferState m => Expr -> m ExprId
infExpr expr = case expr of
    Int p n -> do
        tid <- infGenTypeId
        eid <- infAddExpr expr tid
        infAddDefaultTypeToId tid I64
        return eid

    AST.Bool p b         -> do
        tid <- infGenTypeId
        eid <- infAddExpr expr tid
        infAddTypeToId tid Type.Bool
        return eid

    Infix p op e1 e2
        | op == EqEq || op == OrOr || op == AndAnd -> do
            eid1@(ExprId ei1) <- infExpr e1
            eid2@(ExprId ei2) <- infExpr e2
            tid1 <- infGetTid eid1
            tid2 <- infGetTid eid2
            infAddTypeEq tid1 tid2
            tid <- infGenTypeId
            infAddTypeToId tid Type.Bool
            infAddExpr (Infix p op (Expr ei1) (Expr ei2)) tid

    Infix p op e1 e2
        | op == Plus || op == Times || op == Divide -> do
            eid1@(ExprId ei1) <- infExpr e1
            eid2@(ExprId ei2) <- infExpr e2
            tid1 <- infGetTid eid1
            tid2 <- infGetTid eid2
            infAddTypeEq tid1 tid2
            infAddExpr (Infix p op (Expr ei1) (Expr ei2)) tid1

    Ident (Sym sym) -> do
        Just sid <- SymTab.lookupSymKey sym () <$> gets symTab
        tid <- infGetSymTid sid
        infAddExpr (Ident (Sym sym)) tid


infStmt :: BoM InferState m => Stmt -> m Stmt
infStmt stmt = case stmt of
    Assign pos (PatIdent p sym) expr -> do
        ExprId ei <- infExpr expr
        tid <- infGetTid (ExprId ei) 
        sid <- infAddSym sym tid
        return $ Assign p (PatIdent p $ show sid) (Expr ei)


infResolve :: BoM InferState m => m ()
infResolve = do
    pairs <- gets typeEqPairs
    case pairs of
        (t0, t1) : xs -> substitute t0 t1 >> infResolve
        []            -> return ()

    where
        -- replaced t1 with t2
        substitute :: BoM InferState m => TypeId -> TypeId -> m ()
        substitute t1 t2 = do
            let rep = \tid -> if tid == t1 then t2 else tid
            modify $ \s -> s
                { expressions  = Map.map (\(e, i) -> (e, rep i)) (expressions s)
                , typeDefaults = Map.mapKeys rep (typeDefaults s)
                , typeEqPairs  = [ (rep ta, rep tb) | (ta, tb) <- (typeEqPairs s), rep ta /= rep tb ]
                , symbols      = Map.map (\(s, i) -> (s, rep i)) (symbols s)
                }
        
        

prettyInferState :: InferState -> IO ()
prettyInferState state = do
    putStrLn "Expressions"

    forM_ (Map.toList $ expressions state) $ \(eid, (expr, tid)) ->
        putStrLn $ show eid ++ ":" ++ show tid ++ " " ++ show expr

    putStrLn ""
    putStrLn "Types"

    forM_ (Map.toList $ typeDefs state) $ \(tid, t) ->
        putStrLn $ show tid ++ " " ++ show t

    forM_ (typeEqPairs state) $ \(t1, t2) ->
        putStrLn $ show t1 ++ " = " ++ show t2

    forM_ (Map.toList $ typeDefaults state) $ \(tid, typ) ->
        putStrLn $ show tid ++ " default: " ++ show typ

    putStrLn ""
    putStrLn "Symbols"
    forM_ (Map.toList $ symbols state) $ \(sid, (sym, tid)) ->
        putStrLn $ show sid ++ ":" ++ show tid ++ " " ++ sym

    putStrLn ""
    putStrLn "Symbol Table"
    SymTab.prettySymTab (symTab state)
