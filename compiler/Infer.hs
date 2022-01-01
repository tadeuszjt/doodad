{-# LANGUAGE FlexibleContexts #-}
module Infer where

import System.FilePath
import Control.Monad.State
import Control.Monad.Except hiding (void, fail)
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
    | KeyFunc [Type]
    | KeyType
    deriving (Show, Eq, Ord)


data Object
    = ObjVar
    | ObjFunc Type
    | ObjType
    deriving (Show, Eq)


data Constraint
    = ConsEq Type Type
    | ConsBase Type Type
    | ConsElemType Type Type
    | ConsMemberType Type Type String
    | ConsFieldType Type Type Int
    deriving (Eq, Ord, Show)


data InferState =
    InferState
        { imports      :: Map.Map ModuleName InferState
        , expressions  :: Map.Map ExprId (Expr, Type)
        , defaults     :: Map.Map TypeId Type
        , symTab       :: SymTab.SymTab String SymKey Object
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
        inferPath :: BoM RunInferState m => FilePath -> m InferState
        inferPath path = do
            let modName      = takeFileName path
            let modDirectory = takeDirectory path
            files <- getSpecificModuleFiles modName =<< getBoFilesInDirectory modDirectory
            assert (not $ null files) ("no files for: " ++ path)

            combinedAST <- combineASTs =<< zipWithM parse [0..] files
            importPaths <- forM (AST.astImports combinedAST) $ \importPath ->
                checkAndNormalisePath $ joinPath [modDirectory, importPath]

            flatAST <- snd <$> runBoMTExcept initFlattenState (flattenAST combinedAST)
                

            let importNames = map takeFileName importPaths
            assert (length importNames == length (Set.fromList importNames)) "import name collision"

            importMap <- fmap Map.fromList $ forM importPaths $ \importPath -> do
                state <- runModInfer importPath (Set.insert path pathsVisited)
                return (takeFileName importPath, state)

            fmap snd $ withFiles files $ runBoMTExcept (initInferState importMap) (infAST combinedAST)


define :: BoM InferState m => String -> SymKey -> Object -> m ()
define sym key obj = do
    resm <- SymTab.lookupHead sym key <$> (gets symTab)
    assert (isNothing resm) (sym ++ " already defined")
    modify $ \s -> s { symTab = SymTab.insert sym key obj (symTab s) }


undefine :: BoM InferState m => String -> SymKey -> m ()
undefine sym key = do
    modify $ \s -> s { symTab = SymTab.deleteHead sym key (symTab s) }


look :: BoM InferState m => Symbol -> SymKey -> m Object
look symbol key = do
    resm <- lookm symbol key
    assert (isJust resm) (show symbol ++ " Undefined")
    return (fromJust resm)


lookSym :: BoM InferState m => Symbol -> m [(SymKey, Object)]
lookSym symbol = case symbol of
    Sym sym -> do
        localTabRess <- fmap (SymTab.lookupSym sym) (gets symTab)
        importTabRess <- concat . map (SymTab.lookupSym sym . symTab) . Map.elems <$> gets imports
        return (localTabRess ++ importTabRess)

    SymQualified mod sym -> do
        impm <- Map.lookup mod <$> gets imports
        assert (isJust impm) $ "No module: " ++ mod
        let imp = fromJust impm
        return $ SymTab.lookupSym sym (symTab imp)



lookm :: BoM InferState m => Symbol -> SymKey -> m (Maybe Object)
lookm symbol key = case symbol of
    Sym sym -> do
        localTabResm <- fmap (SymTab.lookup sym key) (gets symTab)
        case localTabResm of
            Just obj -> return (Just obj)
            Nothing -> do
                symTabs <- map symTab . Map.elems <$> gets imports
                let results = catMaybes $ map (SymTab.lookup sym key) symTabs
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
    where
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
infExpr expr = withPos expr $ case expr of
    Int p n              -> addExpr expr =<< genType
    Float p f            -> addExpr expr =<< genType
    AST.Table pos [[]]   -> addExpr expr =<< genType
    AST.Tuple pos [expr] -> infExpr expr
    Range p _ _ _        -> addExpr expr =<< genType
    Null p               -> addExpr expr =<< genType
    AST.Bool p b         -> addExpr expr =<< genType

    AST.Char p c -> do
        t <- genType
        addExpr expr t

    Member pos exp sym    -> do
        e <- infExpr exp
        addExpr (Member pos e sym) =<< genType


    Infix p op expr1 expr2
        | op `elem` [EqEq, OrOr, AndAnd, NotEq, AST.LT, AST.GT, GTEq, LTEq] -> do
            e1 <- infExpr expr1
            e2 <- infExpr expr2

            t1 <- typeOf e1
            t2 <- typeOf e2
            substitute t1 t2

            t <- genType
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

    Ident pos symbol -> do
        objm <- lookm symbol KeyVar
        case objm of
            Nothing     -> fail $ "Undefined symbol: " ++ show symbol
            Just ObjVar -> addExpr (Ident pos symbol) =<< genType

    String pos str -> do
        t <- genType
        addExpr expr t

    Call pos id@(Ident p symbol) exprs -> do
        es <- mapM infExpr exprs
        xs <- lookSym symbol
        case xs of
            --[]                   -> fail $ show symbol ++ " Undefined"
            [(k, ObjFunc retty)] -> addExpr (Call pos id es) retty
            _                    -> addExpr (Call pos id es) =<< genType

    Call pos expr exprs -> do
        e <- infExpr expr
        es <- mapM infExpr exprs
        addExpr (Call pos e es) =<< genType

    AST.Tuple pos exprs -> do
        es <- mapM infExpr exprs
        ts <- mapM typeOf es

        t <- genType
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
        addExpr (Subscript p e1 e2) t

    AST.Table p [exprs] -> do
        es <- mapM infExpr exprs
        ts <- mapM typeOf es
        let ts1 = head ts
        mapM (\x -> substitute x ts1) ts

        t <- genType
        addExpr (AST.Table p [es]) t
        

    _ -> error $ "Cannot infer: " ++ show expr


infPattern :: BoM InferState m => Pattern -> Type -> m Pattern
infPattern pattern exprType = withPos pattern $ case pattern of
    PatIdent _ sym -> do
        define sym KeyVar ObjVar
        return pattern

    PatTuple pos pats -> do
        ps <- forM (zip pats [0..]) $ \(pat, i) -> do
            t <- genType
            infPattern pat t

        return (PatTuple pos ps)

    PatSplitElem pos pat1 pat2 -> do
        t1 <- genType
        p1 <- infPattern pat1 t1
        p2 <- infPattern pat2 exprType
        return $ PatSplitElem pos p1 p2

    PatGuarded pos pat expr -> do
        p <- infPattern pat exprType
        e <- infExpr expr
        t <- typeOf e
        return $ PatGuarded pos p e

    PatTyped pos typ [pat] -> do
        pat' <- infPattern pat exprType
        return $ PatTyped pos typ [pat']
    PatArray _ pats -> return pattern
        
        



    _ -> error $ "Cannot infer pattern: " ++ show pattern


infCondition :: BoM InferState m => Condition -> m Condition
infCondition cnd = case cnd of
    CondExpr expr -> do
        e <- infExpr expr
        t <- typeOf e
        return (CondExpr e)

    CondMatch pat expr -> do
        e <- infExpr expr
        t <- typeOf e
        p <- infPattern pat t
        return (CondMatch p e)


infType :: BoM InferState m => Type -> m Type
infType typ = case typ of
    ADT xs -> do
        forM_ xs $ \(s, t) -> do
            when (t == Void) $ define s KeyVar ObjVar

        return (ADT xs)

    t -> return t


infStmt :: BoM InferState m => Stmt -> m Stmt
infStmt stmt = withPos stmt $ case stmt of
    AST.Typedef pos (Sym sym) typ -> do
        t <- infType typ
        define sym KeyType ObjType
        return $ AST.Typedef pos (Sym sym) t
        
    Assign pos pat expr -> do
        e <- infExpr expr
        infPat <- infPattern pat =<< typeOf e
        return (Assign pos infPat e)

    Set pos index expr -> return stmt

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
        define idxStr KeyVar ObjVar

        t <- genType
        e <- infExpr expr
        infBlk <- infStmt blk

        popSymTab

        return $ For pos idxStr e Nothing infBlk

    Print pos exprs -> do
        Print pos <$> mapM infExpr exprs

    FuncDef pos (Sym sym) params retty blk -> withCurRetty retty $ do
        look (Sym sym) (KeyFunc $ map paramType params)

        pushSymTab
        forM_ params $ \(Param p s t) -> do
            define s KeyVar ObjVar

        blk' <- infStmt blk
        popSymTab
        return $ FuncDef pos (Sym sym) params retty blk'

    Extern pos name sym params retty -> do
        define sym (KeyFunc $ map paramType params) (ObjFunc retty)
        return $ Extern pos name sym params retty

    If p cnd blk Nothing -> do
        cnd' <- infCondition cnd
        blk' <- infStmt blk
        return $ If p cnd' blk' Nothing

    Return p (Just expr) -> do
        curRetty <- gets curRetty
        assert (curRetty /= Void) "Cannot return in void function"

        e <- infExpr expr
        t <- typeOf e
        substitute t curRetty
        return $ Return p (Just e)

    AppendStmt append -> return stmt

    Switch p expr cases -> return stmt

    For p idxStr expr guardm blk -> return stmt


    CallStmt p _ _ -> return stmt

    _ -> error $ "Cannot infer: " ++ show stmt



infTopFuncDef :: BoM InferState m => Stmt -> m ()
infTopFuncDef (FuncDef pos (Sym sym) params retty _) = withPos pos $ do
    define sym (KeyFunc $ map paramType params) (ObjFunc retty)
    undefine sym KeyVar
    define sym KeyVar ObjVar
    


infAST :: BoM InferState m => AST -> m AST
infAST ast = do
    let typeDefStmts = [ x | x@(AST.Typedef _ _ _) <- astStmts ast ]
    let funcDefStmts = [ x | x@(AST.FuncDef _ _ _ _ _) <- astStmts ast ]

    mapM infTopFuncDef funcDefStmts




    stmts' <- mapM infStmt (astStmts ast)



    let ast' = AST {
        astModuleName = astModuleName ast,
        astImports = astImports ast,
        astStmts = stmts'
    }
    return ast'


infResolve :: BoM InferState m => m ()
infResolve = do
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
    putStrLn "Symbol Table"
    SymTab.prettySymTab (symTab state)
