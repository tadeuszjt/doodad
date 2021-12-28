{-# LANGUAGE FlexibleContexts #-}
module Infer where

import System.FilePath
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
import Modules
import Flatten hiding (imports)

newtype TypeId = TypeId Int
    deriving (Eq, Ord)

newtype ExprId = ExprId Int
    deriving (Eq, Ord)

instance Show TypeId where show (TypeId i) = 't' : show i
instance Show ExprId where show (ExprId i) = 'e' : show i


data SymKey
    = KeyVar
    deriving (Show, Eq, Ord)


data Object
    = ObjVar
    deriving (Show, Eq)


data Constraint
    = ConsEq Type Type
    | ConsBase Type Type
    | ConsElemType Type Type
    | ConsMemberType Type Type String
    deriving (Eq, Ord, Show)


data InferState =
    InferState
        { imports      :: Map.Map ModuleName InferState
        , expressions  :: Map.Map ExprId (Expr, Type)
        , defaults     :: Map.Map TypeId Type
        , symTab       :: SymTab.SymTab String SymKey Object
        , constraints  :: [Constraint]
        , exprIdSupply :: Int
        , typeIdSupply :: Int
        , symIdSupply  :: Int
        , curRetty     :: Type
        }
    deriving (Show)


initInferState imp = InferState
    { imports      = imp
    , expressions  = Map.empty
    , defaults     = Map.empty
    , symTab       = SymTab.initSymTab
    , constraints  = []
    , exprIdSupply = 0
    , typeIdSupply = 0
    , symIdSupply  = 0
    , curRetty     = Void
    }


data RunInferState
    = RunInferState
        { modInferMap :: Map.Map FilePath InferState
        }


initRunInferState = RunInferState { modInferMap = Map.empty }


runModInfer :: BoM RunInferState m => FilePath -> Set.Set FilePath -> m InferState
runModInfer modPath pathsVisited = do
    path <- checkAndNormalisePath modPath
    assert (not $ Set.member path pathsVisited) ("importing: " ++ path ++ " forms a cycle")
    resm <- Map.lookup path <$> gets modInferMap
    maybe (inferPath path) (return) resm
    where
        assert b s = when (not b) (fail s)

        inferPath :: BoM RunInferState m => FilePath -> m InferState
        inferPath path = do
            let modName      = takeFileName path
            let modDirectory = takeDirectory path
            files <- getSpecificModuleFiles modName =<< getBoFilesInDirectory modDirectory
            assert (not $ null files) ("no files for: " ++ path)

            combinedAST <- combineASTs =<< zipWithM parse [0..] files
            importPaths <- forM (AST.astImports combinedAST) $ \importPath ->
                checkAndNormalisePath $ joinPath [modDirectory, importPath]

            let importNames = map takeFileName importPaths
            assert (length importNames == length (Set.fromList importNames)) "import name collision"


            importMap <- fmap Map.fromList $ forM importPaths $ \importPath -> do
                state <- runModInfer importPath (Set.insert path pathsVisited)
                return (takeFileName importPath, state)

            res <- runBoMT (initInferState importMap) (infAST combinedAST)
            case res of
                Left e -> error (show e)
                Right (_, x) -> return x


define :: BoM InferState m => String -> SymKey -> Object -> m ()
define sym key obj = do
    resm <- SymTab.lookupSymKey sym key <$> (gets symTab)
    when (isJust resm) $ fail "Already defined"
    modify $ \s -> s { symTab = SymTab.insert sym key obj (symTab s) }


lookm :: BoM InferState m => Symbol -> SymKey -> m (Maybe Object)
lookm symbol key = case symbol of
    Sym sym -> do
        localTabResm <- fmap (SymTab.lookupSymKey sym key) (gets symTab)
        case localTabResm of
            Just obj -> return (Just obj)
            Nothing -> do
                symTabs <- map symTab . Map.elems <$> gets imports
                let results = catMaybes $ map (SymTab.lookupSymKey sym key) symTabs
                case results of
                    []  -> return Nothing
                    [x] -> return (Just x)
                    _   -> error "More than one definition"

    SymQualified mod sym -> do
        error "SymQualified"

    _ -> error "lookm"



addExpr :: BoM InferState m => Expr -> Type -> m Expr
addExpr expr typ = do
    id <- gets exprIdSupply
    modify $ \s -> s { expressions = Map.insert (ExprId id) (expr, typ) (expressions s) }
    modify $ \s -> s { exprIdSupply = id + 1 }
    return (Expr id)


addDefault :: BoM InferState m => TypeId -> Type -> m ()
addDefault tid typ = do
    modify $ \s -> s { defaults = Map.insert tid typ (defaults s) }


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


substitute :: BoM InferState m => Type -> Type -> m ()
substitute t1 t2 = do
    let f = \t -> if t == t1 then t2 else t
    modify $ \s -> s { expressions = Map.map (\(e, t) -> (e, mapType f t)) (expressions s) }
    modify $ \s -> s { constraints = map (mapConstraint f) (constraints s) }
    where
        mapConstraint :: (Type -> Type) -> Constraint -> Constraint
        mapConstraint f con = case con of
            ConsEq t1 t2 -> ConsEq (mapType f t1) (mapType f t2)
            ConsBase t1 t2 -> ConsBase (mapType f t1) (mapType f t2)
            ConsElemType t1 t2 -> ConsElemType (mapType f t1) (mapType f t2)
            ConsMemberType t1 t2 sym -> ConsMemberType (mapType f t1) (mapType f t2) sym
    
        mapType :: (Type -> Type) -> Type -> Type
        mapType f typ = case typ of
            t | isSimple t  -> f t
            Type.Type id    -> f typ
            Type.Void       -> f typ
            Type.Typedef _  -> f $ typ
            Type.Tuple xs   -> f $ Type.Tuple [(s, mapType f t) | (s, t) <- xs]
            Type.ADT xs     -> f $ Type.ADT   [(s, mapType f t) | (s, t) <- xs]
            Type.Table ts   -> f $ Type.Table [mapType f t | t <- ts]
            Type.Func ts rt -> f $ Type.Func  [mapType f t | t <- ts] (mapType f rt)
            _ -> error $ show typ


constrain :: BoM InferState m => Constraint -> m ()
constrain con = do
    cons <- gets constraints
    when (not $ elem con cons) $
        modify $ \s -> s { constraints = con : (constraints s) }


typeOf :: BoM InferState m => Expr -> m Type
typeOf expr = do
    case expr of
        Expr id -> snd <$> (Map.! (ExprId id)) <$> gets expressions
        _       -> fail $ "Cannot get typeOf: " ++ show expr


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
    Null p               -> addExpr expr =<< genType

    AST.Char p c -> do
        t <- genType
        constrain (ConsBase t Type.Char)
        addExpr expr t

    Member p exp sym    -> do
        e <- infExpr exp
        et <- typeOf e
        t <- genType
        constrain $ ConsMemberType t et sym
        addExpr (Member p e sym) t

    AST.Bool p b -> do
        t <- genType
        constrain (ConsBase t Type.Bool)
        addExpr expr t

    Infix p op expr1 expr2
        | op `elem` [EqEq, OrOr, AndAnd, NotEq, AST.LT, AST.GT, GTEq, LTEq] -> do
            e1 <- infExpr expr1
            e2 <- infExpr expr2

            t1 <- typeOf e1
            t2 <- typeOf e2
            substitute t1 t2

            t <- genType
            constrain (ConsBase t Type.Bool)
            addExpr (Infix p op e1 e2) t

    Prefix p op exp -> do
        e <- infExpr exp
        t <- typeOf e
        addExpr (Prefix p op e) t

    Infix p op expr1 expr2
        | op == Plus || op == Times || op == Divide || op == Minus -> do
            e1 <- infExpr expr1
            e2 <- infExpr expr2

            t1 <- typeOf e1
            t2 <- typeOf e2
            substitute t1 t2
            addExpr (Infix p op e1 e2) t2

    Ident symbol -> do
        objm <- lookm symbol KeyVar
        case objm of
            Nothing -> error $ show symbol
            Just ObjVar -> addExpr (Ident symbol) =<< genType

    String pos str -> do
        t <- genType
        constrain $ ConsBase t $ Type.Table [Type.Char]
        addExpr expr t

    Call pos expr exprs -> do
        e <- infExpr expr
        es <- mapM infExpr exprs
        addExpr (Call pos e es) =<< genType

    AST.Tuple pos exprs -> do
        es <- mapM infExpr exprs
        ts <- mapM typeOf es

        t <- genType
        constrain $ ConsBase t (Type.Tuple [("", t) | t <- ts])
        addExpr (AST.Tuple pos es) t

    Conv p typ exprs -> do
        es <- mapM infExpr exprs
        addExpr (Conv p typ es) =<< genType

    Copy p expr -> do
        e <- infExpr expr
        addExpr (Copy p e) =<< typeOf e

    Len p expr -> do
        e <- infExpr expr
        addExpr (Len p e) =<< genType

    Subscript p expr1 expr2 -> do
        e1 <- infExpr expr1
        e2 <- infExpr expr2
        t1 <- typeOf e1
        t <- genType
        constrain (ConsElemType t t1)
        addExpr (Subscript p e1 e2) t

    AST.Table p [exprs] -> do
        es <- mapM infExpr exprs
        ts <- mapM typeOf es
        let ts1 = head ts
        mapM (\x -> substitute x ts1) ts

        t <- genType
        constrain (ConsElemType ts1 t)
        addExpr (AST.Table p [es]) t
        

    _ -> error $ "Cannot infer: " ++ show expr


infPattern :: BoM InferState m => Pattern -> Expr -> m Pattern
infPattern pattern expr@(Expr _) = case pattern of
    PatIdent p sym -> do
        define sym KeyVar ObjVar
        return pattern

    PatTuple p pats -> do
        tupT <- genType
        tupTs <- forM pats (\_ -> genType)

        let tupType = Type.Tuple [ ("", typ) | typ <- tupTs ]
        constrain (ConsBase tupT tupType)

        t <- typeOf expr
        substitute t tupT

        pats' <- forM (zip3 pats tupTs [0..]) $ \(pat, ft,  i) ->
            infPattern pat =<< addExpr (TupleIndex p expr i) ft

        return (PatTuple p pats')

    _ -> return pattern


    _ -> error $ "Cannot infer pattern: " ++ show pattern


infCondition :: BoM InferState m => Condition -> m Condition
infCondition cnd = case cnd of
    CondExpr expr -> do
        e <- infExpr expr
        t <- typeOf e
        constrain (ConsBase t Type.Bool)
        return (CondExpr e)

    CondMatch pat expr -> do
        e <- infExpr expr
        p <- infPattern pat e
        return (CondMatch p e)


infStmt :: BoM InferState m => Stmt -> m Stmt
infStmt stmt = case stmt of
    AST.Typedef pos symbol typ -> do
        return $ AST.Typedef pos symbol typ
        
    Assign pos pat expr -> do
        e <- infExpr expr
        infPat <- infPattern pat e
        return (Assign pos infPat e)

    Set pos index expr -> return stmt

    FuncDef pos symbol [] Void blk -> do
        pushSymTab
        infBlk <- infStmt blk
        popSymTab
        return $ FuncDef pos symbol [] Void infBlk

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
        e <- infExpr expr
        infBlk <- infStmt blk

        popSymTab

        return $ For pos idxStr e Nothing infBlk

    Print pos exprs -> do
        Print pos <$> mapM infExpr exprs

    FuncDef pos sym params retty blk -> withCurRetty retty $ do
        pushSymTab
        
--        forM_ params $ \(Param p ps pt) -> do
--            addSym (Sym ps) pt 


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

        e <- infExpr expr
        t <- typeOf e
        substitute t curRetty
        return $ Return p (Just e)

    AppendStmt append -> return stmt

    Switch p expr cases -> return stmt

    For p idxStr expr guardm blk -> return stmt

    Extern p _ _ _ _ -> return stmt

    CallStmt p _ _ -> return stmt

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

        eatStack :: BoM InferState m => m ()
        eatStack = do
            resm <- head <$> gets constraints
            return ()


prettyInferState :: InferState -> IO ()
prettyInferState state = do
    putStrLn "Imports"
    forM_ (Map.toList $ imports state) $ \(name, st) ->
        putStrLn name

    putStrLn ""
    putStrLn "Expressions"
    forM_ (Map.toList $ expressions state) $ \(eid, (expr, tid)) ->
        putStrLn $ show eid ++ ":" ++ show tid ++ " " ++ show expr

    putStrLn ""
    putStrLn "Constraints"
    forM_ (constraints state) $ \cons ->
        putStrLn $ show cons

    putStrLn ""
    putStrLn "Symbol Table"
    SymTab.prettySymTab (symTab state)
