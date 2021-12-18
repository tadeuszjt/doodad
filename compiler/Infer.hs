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

initInferState = InferState
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


addExpr :: BoM InferState m => Expr -> TypeId -> m Expr
addExpr expr tid = do
    id <- gets exprIdSupply
    modify $ \s -> s { expressions = Map.insert (ExprId id) (expr, tid) (expressions s) }
    modify $ \s -> s { exprIdSupply = id + 1 }
    return (Expr id)


genTypeId :: BoM InferState m => m TypeId
genTypeId = do
    n <- gets typeIdSupply
    modify $ \s -> s { typeIdSupply = n + 1 }
    return (TypeId n)


addTypeEq :: BoM InferState m => TypeId -> TypeId -> m ()
addTypeEq t1 t2 = do
    modify $ \s -> s { typeEqPairs = (t1, t2) : (typeEqPairs s) }


addTypeToId:: BoM InferState m => TypeId -> Type -> m ()
addTypeToId tid typ = do
    tm <- Map.lookup tid <$> gets typeDefs
    when (isJust tm) $ fail $ show tid ++ " already has type definition"
    modify $ \s -> s { typeDefs = Map.insert tid typ (typeDefs s) }
    

addDefaultTypeToId :: BoM InferState m => TypeId -> Type -> m ()
addDefaultTypeToId tid typ =
    modify $ \s -> s { typeDefaults = Map.insert tid typ (typeDefaults s) }


getExprTid :: BoM InferState m => Expr -> m TypeId
getExprTid (Expr id) = do
    (_, tid) <- (Map.! (ExprId id)) <$> gets expressions
    return tid

infGetSymTid :: BoM InferState m => SymId -> m TypeId
infGetSymTid sid = do
    (_, tid) <- (Map.! sid) <$> gets symbols
    return tid

addSym :: BoM InferState m => String -> TypeId -> m SymId
addSym sym tid = do
    id <- gets symIdSupply
    modify $ \s -> s { symIdSupply = id + 1 }
    modify $ \s -> s { symbols = Map.insert (SymId id) (sym, tid) (symbols s) }
    modify $ \s -> s { symTab = SymTab.insert sym () (SymId id) (symTab s) }
    return (SymId id)


pushSymTab :: BoM InferState m => m ()
pushSymTab = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: BoM InferState m => m ()
popSymTab = do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


infExpr :: BoM InferState m => Expr -> m Expr
infExpr expr = case expr of
    Int p n -> do
        tid <- genTypeId
        addDefaultTypeToId tid I64
        addExpr expr tid

    Float p f -> do
        tid <- genTypeId
        addDefaultTypeToId tid F64
        addExpr expr tid

    AST.Bool p b -> do
        tid <- genTypeId
        addTypeToId tid Type.Bool
        addExpr expr tid


    Infix p op e1 e2
        | op == EqEq || op == OrOr || op == AndAnd -> do
            e1' <- infExpr e1
            e2' <- infExpr e2

            tid1 <- getExprTid e1'
            tid2 <- getExprTid e2'
            addTypeEq tid1 tid2

            tid <- genTypeId
            addTypeToId tid Type.Bool
            addExpr (Infix p op e1' e2') tid

    Infix p op e1 e2
        | op == Plus || op == Times || op == Divide || op == Minus -> do
            e1' <- infExpr e1
            e2' <- infExpr e2

            tid1 <- getExprTid e1'
            tid2 <- getExprTid e2'
            addTypeEq tid1 tid2
            addExpr (Infix p op e1' e2') tid1

    Ident (Sym sym) -> do
        res <- SymTab.lookupSymKey sym () <$> gets symTab
        sid <- case res of
            Just sid -> return sid
            Nothing -> error $ "Cannot find symbol: " ++ sym
        tid <- infGetSymTid sid

        addExpr (Ident (Sym sym)) tid

    String pos str -> do
        tid <- genTypeId
        addTypeToId tid $ Type.Table [Type.Char]
        addExpr expr tid

    Call pos expr@(Ident _) exprs -> do
        es' <- mapM infExpr exprs
        addExpr (Call pos expr es') =<< genTypeId


    Call pos expr exprs -> do
        e' <- infExpr expr
        es' <- mapM infExpr exprs
        addExpr (Call pos e' es') =<< genTypeId

    AST.Table pos [[]] -> do
        addExpr (AST.Table pos []) =<< genTypeId

    AST.Tuple pos [expr] -> do
        infExpr expr

    Conv p typ [expr] -> do
        e' <- infExpr expr
        addExpr (Conv p typ [e']) =<< genTypeId


    _ -> error $ "Cannot infer: " ++ show expr


infPattern :: BoM InferState m => Pattern -> Expr -> m Pattern
infPattern pattern expr@(Expr _) = case pattern of
    PatIdent p sym -> do
        tid <- getExprTid expr
        addSym sym tid
        return pattern

    PatTuple p pats -> do
        tupTid       <- genTypeId
        tupFieldTids <- forM pats (\_ -> genTypeId)

        let tupType = Type.Tuple [ ("", Type id) | TypeId id <- tupFieldTids ]
        addTypeToId tupTid tupType

        tid <- getExprTid expr
        addTypeEq tid tupTid

        pats' <- forM (zip3 pats tupFieldTids [0..]) $ \(pat, ftid, i) -> do
            t <- genTypeId
            e <- addExpr (TupleIndex p expr i) t
            addTypeEq t ftid
            infPattern pat e

        return (PatTuple p pats')


        

    _ -> error $ "Cannot infer pattern: " ++ show pattern


infStmt :: BoM InferState m => Stmt -> m Stmt
infStmt stmt = case stmt of
    Assign pos pat expr -> do
        e' <- infExpr expr
        infPat <- infPattern pat e'
        return $ Assign pos infPat e'

    FuncDef pos sym [] Void blk -> do
        pushSymTab
        infBlk <- infStmt blk
        popSymTab
        return $ FuncDef pos sym [] Void infBlk

    Block blk -> do
        pushSymTab
        r <- Block <$> mapM infStmt blk
        popSymTab
        return r

    While pos cnd blk -> do
        --infCnd
        While pos cnd <$> infStmt blk

    For pos idxStr expr Nothing blk -> do
        pushSymTab

        idxTid <- genTypeId
        addDefaultTypeToId idxTid I64
        addSym idxStr idxTid

        e' <- infExpr expr
        infBlk <- infStmt blk

        popSymTab

        return $ For pos idxStr e' Nothing infBlk

    Print pos exprs -> do
        Print pos <$> mapM infExpr exprs

    _ -> error $ "Cannot infer: " ++ show stmt


infAST :: BoM InferState m => AST -> m ()
infAST ast = do
    mapM_ infStmt (astStmts ast)


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
