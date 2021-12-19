{-# LANGUAGE FlexibleContexts #-}
module Infer where

import Control.Monad.State
import Data.Maybe
import Data.Word
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

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
        { expressions  :: Map.Map ExprId (Expr, Type)
        , symbols      :: Map.Map SymId (Symbol, Type)
        , constraints  :: Set.Set (Type, Type)
        , symTab       :: SymTab.SymTab Symbol () SymId
        , exprIdSupply :: Int
        , typeIdSupply :: Int
        , symIdSupply  :: Int
        , curRetty     :: Type
        }
    deriving (Show)

initInferState = InferState
    { expressions  = Map.empty
    , symbols      = Map.empty
    , constraints  = Set.empty
    , symTab       = SymTab.initSymTab
    , exprIdSupply = 0
    , typeIdSupply = 0
    , symIdSupply  = 0
    , curRetty     = Void
    }


addExpr :: BoM InferState m => Expr -> Type -> m Expr
addExpr expr typ = do
    id <- gets exprIdSupply
    modify $ \s -> s { expressions = Map.insert (ExprId id) (expr, typ) (expressions s) }
    modify $ \s -> s { exprIdSupply = id + 1 }
    return (Expr id)


genType :: BoM InferState m => m Type
genType = do
    n <- gets typeIdSupply
    modify $ \s -> s { typeIdSupply = n + 1 }
    return (Type n)


withCurRetty :: BoM InferState m => Type -> m a -> m a
withCurRetty typ m = do
    oldRetty <- gets curRetty
    modify $ \s -> s { curRetty = typ }
    r <- m
    modify $ \s -> s { curRetty = oldRetty }
    return r


constrain :: BoM InferState m => Type -> Type -> m ()
constrain t1 t2 = do
    modify $ \s -> s { constraints = Set.insert (t1, t2) (constraints s) }


typeOf :: BoM InferState m => Expr -> m Type
typeOf expr = do
    case expr of
        Expr id -> snd <$> (Map.! (ExprId id)) <$> gets expressions
        _       -> fail $ "Cannot get typeOf: " ++ show expr


typeOfSym :: BoM InferState m => Symbol -> m Type
typeOfSym (Symbol id) =
    snd . (Map.! (SymId id)) <$> gets symbols


addSym :: BoM InferState m => Symbol -> Type -> m Symbol
addSym symbol typ = do
    resm <- SymTab.lookupHead symbol () <$> gets symTab
    when (isJust resm) $ fail $ show symbol ++ " already defined"


    id <- gets symIdSupply
    modify $ \s -> s { symIdSupply = id + 1 }
    modify $ \s -> s { symbols = Map.insert (SymId id) (symbol, typ) (symbols s) }
    modify $ \s -> s { symTab = SymTab.insert symbol () (SymId id) (symTab s) }
    return (Symbol id)


pushSymTab :: BoM InferState m => m ()
pushSymTab = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: BoM InferState m => m ()
popSymTab = do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


infExpr :: BoM InferState m => Expr -> m Expr
infExpr expr = case expr of
    Int p n              -> addExpr expr =<< genType
    Float p f            -> addExpr expr =<< genType
    AST.Table pos [[]]   -> addExpr expr =<< genType
    AST.Tuple pos [expr] -> infExpr expr
    Range p _ _ _        -> addExpr expr =<< genType

    AST.Bool p b -> do
        t <- genType
        constrain t Type.Bool
        addExpr expr t

    Infix p op expr1 expr2
        | op == EqEq || op == OrOr || op == AndAnd || op == NotEq -> do
            e1 <- infExpr expr1
            e2 <- infExpr expr2

            t1 <- typeOf e1
            t2 <- typeOf e2
            constrain t1 t2

            t <- genType
            constrain t Type.Bool
            addExpr (Infix p op e1 e2) t

    Infix p op expr1 expr2
        | op == Plus || op == Times || op == Divide || op == Minus -> do
            e1 <- infExpr expr1
            e2 <- infExpr expr2

            t1 <- typeOf e1
            t2 <- typeOf e2
            constrain t1 t2
            addExpr (Infix p op e1 e2) t2

    Ident symbol -> do
        res <- SymTab.lookupSymKey symbol () <$> gets symTab
        s <- case res of
            Just (SymId id) -> return (Symbol id)
            Nothing         -> addSym symbol =<< genType

        addExpr (Ident s) =<< typeOfSym s


    String pos str -> do
        t <- genType
        constrain t $ Type.Table [Type.Char]
        addExpr expr t

    Call pos expr exprs -> do
        e <- infExpr expr
        es <- mapM infExpr exprs
        addExpr (Call pos e es) =<< genType

    AST.Tuple pos exprs -> do
        es <- mapM infExpr exprs
        ts <- mapM typeOf es

        t <- genType
        constrain t $ Type.Tuple [("", t) | t <- ts]
        addExpr (AST.Tuple pos es) t

    Conv p typ exprs -> do
        es <- mapM infExpr exprs
        addExpr (Conv p typ es) =<< genType

    Copy p expr -> do
        e <- infExpr expr
        t <- genType
        constrain t =<< typeOf e
        addExpr (Copy p e) t

    Len p expr -> do
        e <- infExpr expr
        addExpr (Len p e) =<< genType

    Subscript p expr1 expr2 -> do
        e1 <- infExpr expr1
        e2 <- infExpr expr2
        addExpr (Subscript p e1 e2) =<< genType

    _ -> error $ "Cannot infer: " ++ show expr


infPattern :: BoM InferState m => Pattern -> Expr -> m Pattern
infPattern pattern expr@(Expr _) = case pattern of
    PatIdent p sym -> do
        addSym (Sym sym) =<< typeOf expr
        return pattern

    PatTuple p pats -> do
        tupT <- genType
        tupTs <- forM pats (\_ -> genType)

        let tupType = Type.Tuple [ ("", typ) | typ <- tupTs ]
        constrain tupT tupType

        t <- typeOf expr
        constrain t tupT

        pats' <- forM (zip3 pats tupTs [0..]) $ \(pat, ft,  i) -> do
            t <- genType
            e <- addExpr (TupleIndex p expr i) t
            constrain t ft
            infPattern pat e

        return (PatTuple p pats')


    _ -> error $ "Cannot infer pattern: " ++ show pattern


infCondition :: BoM InferState m => Condition -> m Condition
infCondition (CondExpr expr) = do
    e' <- infExpr expr
    t <- typeOf e'
    constrain t Type.Bool
    return (CondExpr e')


infStmt :: BoM InferState m => Stmt -> m Stmt
infStmt stmt = case stmt of
    Assign pos pat expr -> do
        e' <- infExpr expr
        infPat <- infPattern pat e'
        return $ Assign pos infPat e'

    Set pos index expr -> return stmt

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

        t <- genType
        addSym (Sym idxStr) t

        e' <- infExpr expr
        infBlk <- infStmt blk

        popSymTab

        return $ For pos idxStr e' Nothing infBlk

    Print pos exprs -> do
        Print pos <$> mapM infExpr exprs

    FuncDef pos sym params retty blk -> withCurRetty retty $ do
        pushSymTab
        
        forM_ params $ \(Param p ps pt) -> do
            t <- genType
            sym <- addSym (Sym ps) t
            constrain t pt


        blk' <- infStmt blk
        popSymTab
        return $ FuncDef pos sym params retty blk'

    If p cnd blk Nothing -> do
        cnd' <- infCondition cnd
        blk' <- infStmt blk
        return $ If p cnd' blk' Nothing



    Return p (Just expr) -> do
        curRetty <- gets curRetty
        when (curRetty == Void) $ fail $ "Cannot return in void function"

        e' <- infExpr expr
        t <- typeOf e'
        constrain t curRetty

        return $ Return p (Just e')

    AppendStmt append -> return stmt

    Switch p expr cases -> return stmt

    For p idxStr expr guardm blk -> return stmt

    _ -> error $ "Cannot infer: " ++ show stmt


infAST :: BoM InferState m => AST -> m AST
infAST ast = do
    stmts' <- mapM infStmt (astStmts ast)
    let ast' = AST {
        astModuleName = astModuleName ast,
        astImports = astImports ast,
        astStmts = stmts'
    }
    return ast'


infResolve :: BoM InferState m => m ()
infResolve = do
    eatStack

    where
        -- replace t1 with t2
        substitute :: BoM InferState m => Type -> Type -> m ()
        substitute t1 t2 = do
            let f = \t -> if t == t1 then t2 else t

            modify $ \s -> s { expressions = Map.map (\(e, t) -> (e, mapType f t)) (expressions s) }
            modify $ \s -> s { symbols     = Map.map (\(s, t) -> (s, mapType f t)) (symbols s) }
            modify $ \s -> s { constraints = Set.map (\(ta, tb) -> (mapType f ta, mapType f tb)) (constraints s) }

        mapType :: (Type -> Type) -> Type -> Type
        mapType f typ = case typ of
            Type.Type id  -> f typ
            Type.I64      -> f typ
            Type.F64      -> f typ
            Type.Char     -> f typ
            Type.Bool     -> f typ
            Type.Tuple xs -> f $ Type.Tuple [(s, mapType f t) | (s, t) <- xs]
            Type.ADT xs   -> f $ Type.ADT   [(s, mapType f t) | (s, t) <- xs]
            Type.Table ts -> f $ Type.Table [(mapType f t) | t <- ts]
            _ -> error $ show typ

        eatStack :: BoM InferState m => m ()
        eatStack = do
            resm <- Set.lookupMin <$> gets constraints

            case resm of
                Nothing -> return ()
                Just res@(a, b) | a == b -> do
                    modify $ \s -> s { constraints = Set.delete res (constraints s) }
                    eatStack
                Just res@(Type a, b) -> do
                    substitute (Type a) b
                    modify $ \s -> s { constraints = Set.delete res (constraints s) }
                    eatStack

                Just res@(Type.Tuple axs, Type.Tuple bxs) -> do
                    when (length axs /= length bxs) $ fail "Error Stack"
                    forM_ (zip axs bxs) $ \((as, at), (bs, bt)) -> do
                        substitute at bt
                    eatStack


                Just res -> error $ "Stack elem: " ++ show res


prettyInferState :: InferState -> IO ()
prettyInferState state = do
    putStrLn "Expressions"

    forM_ (Map.toList $ expressions state) $ \(eid, (expr, tid)) ->
        putStrLn $ show eid ++ ":" ++ show tid ++ " " ++ show expr

    putStrLn ""
    putStrLn "Types"

    forM_ (constraints state) $ \(t1, t2) ->
        putStrLn $ show t1 ++ " = " ++ show t2

    putStrLn ""
    putStrLn "Symbols"
    forM_ (Map.toList $ symbols state) $ \(sid, (symbol, tid)) ->
        putStrLn $ show sid ++ ":" ++ show tid ++ " " ++ show symbol

    putStrLn ""
    putStrLn "Symbol Table"
    SymTab.prettySymTab (symTab state)
