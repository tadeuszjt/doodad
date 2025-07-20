module InstBuilder where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity

import AST
import Type
import Error


type ID = Int

globalId = 0

data InstBuilderState = InstBuilderState
    { statements    :: Map.Map ID Stmt
    , expressions   :: Map.Map ID Expr
    , patterns      :: Map.Map ID Pattern
    , types         :: Map.Map ID Type
    , textPositions :: Map.Map ID TextPos
    , idSupply      :: ID
    , curId         :: ID
    }

class (Monad m) => MonadInstBuilder m where
    liftInstBuilderState :: State InstBuilderState a -> m a


initInstBuilderState = InstBuilderState
    { statements = Map.singleton globalId (Block [])
    , expressions = Map.empty
    , patterns    = Map.empty
    , types      = Map.empty
    , textPositions = Map.empty
    , idSupply   = globalId + 1
    , curId      = globalId
    }


typeOfPat :: InstBuilderState -> PatternID -> Type
typeOfPat state id = (types state) Map.! id


typeOfExpr :: InstBuilderState -> ExprID -> Type
typeOfExpr state id = (types state) Map.! id


typeOfType :: InstBuilderState -> Type -> Type
typeOfType state (Type id) = (types state) Map.! id


generateId :: MonadInstBuilder m => m ID
generateId = liftInstBuilderState $ do
    idSupply <- gets idSupply
    modify $ \s -> s { idSupply = idSupply + 1 }
    return idSupply


getCurId :: MonadInstBuilder m => m ID
getCurId = do
    liftInstBuilderState (gets curId)


appendId :: MonadInstBuilder m => ID -> m ()
appendId id = do
    curId <- liftInstBuilderState (gets curId)
    b <- liftInstBuilderState $ gets (Map.member curId . statements)
    unless (b) (error "")
    stmt <- liftInstBuilderState $ gets $ (Map.! curId) . statements
    stmt' <- case stmt of
        Block ids -> return $ Block (ids ++ [id])
        x -> error (show x)

    liftInstBuilderState $ modify $ \s -> s { statements = Map.insert curId stmt' (statements s) }


newStmt :: MonadInstBuilder m => TextPos -> Stmt -> m ID
newStmt pos stmt = do
    id <- generateId
    liftInstBuilderState $ modify $ \s -> s
        { statements = Map.insert id stmt (statements s)
        , textPositions = Map.insert id pos (textPositions s)
        } 
    return id


newType :: MonadInstBuilder m => ID -> Type -> m ID
newType id typ = do
    --resm <- liftInstBuilderState $ gets $ Map.lookup id . types
    --unless (isNothing resm) (fail $ "id already typed: " ++ show id)
    liftInstBuilderState $ modify $ \s -> s { types = Map.insert id typ (types s) }
    return id


newExpr :: MonadInstBuilder m => TextPos -> Expr -> m ID
newExpr pos expr = do
    id <- generateId
    liftInstBuilderState $ modify $ \s -> s
        { expressions = Map.insert id expr (expressions s)
        , textPositions = Map.insert id pos (textPositions s)
        }
    return id

newPattern :: MonadInstBuilder m => Pattern -> m ID
newPattern pattern = do
    id <- generateId
    liftInstBuilderState $ modify $ \s -> s { patterns = Map.insert id pattern (patterns s) }
    return id

appendStmt :: MonadInstBuilder m => Stmt -> m ID
appendStmt stmt = do
    id <- generateId
    appendId id
    liftInstBuilderState $ modify $ \s -> s { statements = Map.insert id stmt (statements s) }
    return id


withCurId :: MonadInstBuilder m => ID -> (m a) -> m a
withCurId id f = do
    prevId <- liftInstBuilderState (gets curId)
    liftInstBuilderState $ modify $ \s -> s { curId = id }
    a <- f
    liftInstBuilderState $ modify $ \s -> s { curId = prevId }
    return a


deleteExpr :: MonadInstBuilder m => ExprID -> m ()
deleteExpr exprId = do
    b <- liftInstBuilderState $ gets (Map.member exprId . expressions)
    unless b (error "exprId not found")
    liftInstBuilderState $ modify $ \s -> s { expressions = Map.delete exprId (expressions s) }

deletePattern :: MonadInstBuilder m => PatternID -> m ()
deletePattern patId = do
    b <- liftInstBuilderState $ gets (Map.member patId . patterns)
    unless b (error "patId not found")
    liftInstBuilderState $ modify $ \s -> s { patterns = Map.delete patId (patterns s) }

setExpr :: MonadInstBuilder m => ExprID -> Expr -> m ()
setExpr exprId expr = do
    b <- liftInstBuilderState $ gets (Map.member exprId . expressions)
    unless b (error "exprId not found")
    liftInstBuilderState $ modify $ \s -> s { expressions = Map.insert exprId expr (expressions s) }

setPattern :: MonadInstBuilder m => PatternID -> Pattern -> m ()
setPattern patId pattern = do
    b <- liftInstBuilderState $ gets (Map.member patId . patterns)
    unless b (error "patId not found")
    liftInstBuilderState $ modify $ \s -> s { patterns = Map.insert patId pattern (patterns s) }


setStmt :: MonadInstBuilder m => StmtID -> Stmt -> m ()
setStmt stmtId stmt = do
    b <- liftInstBuilderState $ gets (Map.member stmtId . statements)
    unless b (error "stmtId not found")
    liftInstBuilderState $ modify $ \s -> s { statements = Map.insert stmtId stmt (statements s) }


setType :: MonadInstBuilder m => Int -> Type -> m ()
setType id typ = liftInstBuilderState $ do
    modify $ \s -> s { types = Map.insert id typ (types s) }

prettyInst :: InstBuilderState -> IO ()
prettyInst state = do
    putStrLn "statements:"
    forM_ (Map.toList $ statements state) $ \(id, stmt) ->
        putStrLn $ "\t" ++ show id ++ ": " ++ show stmt

    putStrLn "expressions:"
    forM_ (Map.toList $ expressions state) $ \(id, expr) ->
        putStrLn $ "\t" ++ show id ++ ": " ++ show expr

    putStrLn "patterns:"
    forM_ (Map.toList $ patterns state) $ \(id, pat) ->
        putStrLn $ "\t" ++ show id ++ ": " ++ show pat

    putStrLn "types:"
    forM_ (Map.toList $ types state) $ \(id, typ) ->
        putStrLn $ "\t" ++ show id ++ ": " ++ show typ
