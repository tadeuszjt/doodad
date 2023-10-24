{-# LANGUAGE FlexibleContexts #-}
module Collect where

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import Type
import Constraint
import Monad
import Error
import Control.Monad.State
import qualified SymTab
import Symbol
import ASTResolved
import FunctionFinder

import qualified Debug.Trace


type SymTab = SymTab.SymTab Symbol SymKey Object

data SymKey
    = KeyVar
    | KeyField Symbol -- Field belonging to (Typedef Symbol)
    | KeyAdtField
    deriving (Show, Eq, Ord)

data Object
    = ObjVar Type
    | ObjField Int
    | ObjConst S.Expr
    deriving (Show, Eq)

data CollectState
    = CollectState
        { symTab      :: SymTab
        , curRetty    :: Type
        , collected   :: Map.Map Constraint TextPos
        , defaults    :: Map.Map Constraint TextPos
        , curPos      :: TextPos
        , typeSupply  :: Int
        , astResolved :: ASTResolved
        }

initCollectState annotateCount astResolved = CollectState
    { symTab      = SymTab.initSymTab
    , curRetty    = Void
    , collected   = Map.empty
    , defaults    = Map.empty
    , curPos      = TextPos "" 0 0
    , typeSupply  = annotateCount
    , astResolved = astResolved 
    }


genType :: BoM CollectState m => m Type
genType = do
    i <- gets typeSupply
    modify $ \s -> s { typeSupply = i + 1 }
    return $ Type i


collectPos :: (BoM CollectState m, TextPosition t) => t -> m a -> m a
collectPos t m = withPos t $ do
    old <- gets curPos
    modify $ \s -> s { curPos = (textPos t) }
    r <- m
    modify $ \s -> s { curPos = old }
    return r


collect :: BoM CollectState m => Constraint -> m ()
collect constraint = do
    --liftIO $ putStrLn $ "collected: " ++ show constraint
    modify $ \s -> s { collected = Map.insert (constraint) (curPos s) (collected s) }

collectEq :: BoM CollectState m => Type -> Type -> m ()
collectEq t1 t2 = collect $ ConsEq t1 t2

collectDefault :: BoM CollectState m => Type -> Type -> m ()
collectDefault t1 t2 = do
    modify $ \s -> s { defaults = Map.insert (ConsEq t1 t2) (curPos s) (defaults s) }


look :: BoM CollectState m => Symbol -> SymKey -> m Object
look symbol key = do
    rm <- SymTab.lookup symbol key <$> gets symTab
    assert (isJust rm) $ show symbol ++ " " ++ show key ++ " undefined."
    return (fromJust rm)


define :: BoM CollectState m => Symbol -> SymKey -> Object -> m ()
define symbol key obj = do
    resm <- SymTab.lookupHead symbol key <$> gets symTab
    assert (isNothing resm) $ show symbol ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert symbol key obj (symTab s) }


collectAST :: BoM CollectState m => ASTResolved -> m ()
collectAST ast = do
    forM (Map.toList $ ctorDefs ast) $ \(symbol, (typeDefSymbol, i)) -> do
        (typeArgs, typ) <- mapGet typeDefSymbol =<< gets (typeFuncs . astResolved) -- check
        define (Sym $ sym symbol) (KeyField typeDefSymbol) (ObjField i)

    forM (Map.toList $ constDefs ast) $ \(symbol, expr) -> do
        define symbol KeyVar (ObjConst expr)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) ->
        when (funcTypeArgs body == []) $ do
            --liftIO $ putStrLn $ "collecting func: " ++ show symbol
            --liftIO $ prettyFuncBody symbol body
            collectFuncDef symbol body


collectFuncDef :: BoM CollectState m => Symbol -> FuncBody -> m ()
collectFuncDef symbol body = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }
    oldRetty <- gets curRetty
    modify $ \s -> s { curRetty = funcRetty body }
    forM (funcParams body) $ \(S.Param _ symbol t) -> define symbol KeyVar (ObjVar t)
    forM_ (funcArgs body) $ \(S.Param _ symbol t) -> define symbol KeyVar (ObjVar t)
    collectStmt (funcStmt body)
    modify $ \s -> s { curRetty = oldRetty }
    --collectDefault (funcRetty body) Void
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }



collectStmt :: BoM CollectState m => S.Stmt -> m ()
collectStmt stmt = collectPos stmt $ case stmt of
    S.Increment _ expr -> collectExpr expr
    S.Typedef _ _ _ _ -> return ()
    S.FuncDef _ _ _ _ _ _ _ -> return ()
    S.EmbedC _ _ -> return ()
    S.Block stmts -> mapM_ collectStmt stmts
    S.ExprStmt e -> collectExpr e
    S.Const _ symbol expr -> define symbol KeyVar (ObjConst expr)

    S.Return _ mexpr -> do
        retty <- gets curRetty
        case mexpr of
            Nothing -> collectEq Void retty
            Just expr -> do
                collectEq (typeof expr) retty
                collectExpr expr

    S.If _ expr blk melse -> do
        collect $ ConsBase Bool (typeof expr)
        collectExpr expr
        collectStmt blk
        maybe (return ()) collectStmt melse

    S.Assign _ pattern expr -> do
        collectPattern pattern (typeof expr)
        collectExpr expr

    -- only tables use +=, must be table
    S.SetOp _ S.PlusEq expr1 expr2@(S.AExpr t2 (S.Tuple _ es)) -> do
        case es of
            [] -> fail "what"
            [e] -> do
                collectEq (typeof expr1) (typeof expr2)
                collectExpr expr1
                collectExpr expr2
--            es -> do
--                gts <- replicateM (length es) genType
--                collectBase (typeof expr1) $ Table gts
--                forM_ (zip es gts) $ \(e, t) -> do
--                    collectEq (typeof e) $ Table [t]
--                    collectExpr e
--                collectExpr expr1
--                collectEq (typeof expr2) (typeof expr1)

    S.SetOp _ op expr1 expr2 -> do
        collectEq (typeof expr1) (typeof expr2)
        collectExpr expr1
        collectExpr expr2

    S.While _ expr blk -> do
        collectExpr expr
        collect $ ConsBase Bool (typeof expr)
        collectStmt blk

    S.Switch p expr cases -> do
        collectExpr expr
        forM_ cases $ \(pat, stmt) -> do
            collectPattern pat (typeof expr)
            collectStmt stmt

    S.For p expr mpat blk -> do
        when (isJust mpat) $ do
            gt <- genType
            collect $ ConsSubscript (typeof expr) gt
            collectPattern (fromJust mpat) gt

        collectExpr expr
        collectStmt blk

    S.Data p symbol typ mexpr -> do
        define symbol KeyVar (ObjVar typ)
        maybe (return ()) (collectEq typ . typeof) mexpr
        maybe (return ()) collectExpr mexpr
        
    _ -> error (show stmt)


-- collectPattern pattern <with this type of expression trying to match>
collectPattern :: BoM CollectState m => S.Pattern -> Type -> m ()
collectPattern pattern typ = collectPos pattern $ case pattern of
    S.PatIgnore pos -> return ()
    S.PatIdent _ symbol -> do
        define symbol KeyVar (ObjVar typ)

    S.PatLiteral expr -> do 
        collectEq typ (typeof expr)
        collectExpr expr

    S.PatGuarded _ pat expr -> do
        collectPattern pat typ
        collectExpr expr

    S.PatField _ symbol pats -> do
        ast <- gets astResolved
        [symbol'] <- fmap fst $ runBoMTExcept ast (findCtorCandidates symbol)
        (s, i) <- mapGet symbol' . ctorDefs =<< gets astResolved
        gts <- replicateM (length pats) genType
        zipWithM_ collectPattern pats gts
        forM_ (zip gts [0..]) $ \(t, j) -> collect $ ConsAdtField t i j typ


    S.PatTypeField _ t pat -> do
        collectPattern pat t

    S.PatTuple _ pats -> do
        gts <- replicateM (length pats) genType
        collectDefault typ (Tuple $ Record gts)
        collect $ ConsTuple typ gts
        zipWithM_ collectPattern pats gts

    S.PatArray _ pats -> do
        gt <- genType
        mapM_ (\p -> collectPattern p gt) pats
        collect $ ConsMember typ 0 gt

    S.PatAnnotated pat t -> do
        collectEq t typ
        collectPattern pat typ

    S.PatNull _ -> return ()
        
    _ -> error $ show pattern


collectCall :: BoM CollectState m => Type -> [S.Expr] -> Symbol -> [S.Expr] -> m ()
collectCall exprType params symbol args = do -- can be resolved or sym
    let callHeader = FuncHeader [] (map typeof params) symbol (map typeof args) exprType
    ast <- gets astResolved
    candidates <- fmap fst $ runBoMTExcept ast (findCandidates callHeader)
    case candidates of
        [symbol] | isNonGenericFunction symbol ast -> do
            let header = getFunctionHeader symbol ast
            collectEq exprType (returnType header)
            zipWithM_ collectEq (map typeof params) (paramTypes header)
            zipWithM_ collectEq (map typeof args) (argTypes header)

            --liftIO $ putStrLn $ "collected non-generic: " ++ show symbol
            
        _ -> return ()

    mapM_ collectExpr params
    mapM_ collectExpr args


collectExpr :: BoM CollectState m => S.Expr -> m ()
collectExpr (S.AExpr exprType expr) = collectPos expr $ case expr of
    S.Call _ ps s es   -> collectCall exprType ps s es
    S.Prefix _ op e    -> collectEq exprType (typeof e) >> collectExpr e
    S.Int _ c          -> collectDefault exprType I64
    S.Float _ f        -> collectDefault exprType F64
    S.Null _           -> return ()
    S.Construct _ _ es -> mapM_ collectExpr es
    S.String _ _       -> collectDefault exprType $ String

    S.Builtin _ ps sym es -> do 
        case sym of
            "conv" -> do 
                assert (length ps == 0) "invalid conv"
            "len" -> collectDefault exprType I64
            "print" -> collectEq exprType Void
        mapM_ collectExpr ps
        mapM_ collectExpr es

    S.Char _ c       -> do
        collect $ ConsBase exprType Char
        collectDefault exprType Char

    S.Bool _ b       -> do
        collect $ ConsBase exprType Bool
        collectDefault exprType Bool

    S.Ident _ symbol -> do
        obj <- look symbol KeyVar
        case obj of
            ObjVar t -> collectEq t exprType
            ObjConst e -> do -- special!
                return ()
--                count <- gets typeSupply
--                (e', count') <- runBoMTExcept count (annotate e)
--                modify $ \s -> s { typeSupply = count' }
--                collectEq (typeof e') exprType
--                collectExpr e'
                

    S.Infix _ op e1 e2 -> do
        case op of
            _ | op `elem` [S.Plus, S.Minus, S.Times, S.Divide, S.Modulo] -> do
                collectEq exprType (typeof e1)
            _ | op `elem` [S.LT, S.GT, S.LTEq, S.GTEq, S.EqEq, S.NotEq]  -> do
                collect $ ConsBase exprType Bool
                collectDefault exprType Bool
            _ | op `elem` [S.AndAnd, S.OrOr] -> do
                collect $ ConsBase exprType Bool
                collectEq exprType (typeof e1)
            _ -> return ()
                    
        collectEq (typeof e1) (typeof e2)
        collectExpr e1
        collectExpr e2

    S.Subscript _ e1 e2 -> do
        collect $ ConsSubscript (typeof e1) exprType
        collectExpr e1
        collectExpr e2
        --collectDefault (typeof e2) I64

    S.Tuple _ es -> do
        collect $ ConsTuple exprType (map typeof es)
--        collect $ ConsBase exprType $ Tuple $ Record (map typeof es)
--        collectDefault exprType $ Tuple (map typeof es)
        mapM_ collectExpr es

    S.Field _ e (Sym sym) -> do
        case typeof e of
            Type x         -> return ()
            TypeApply symbol _ -> do
                ObjField i  <- look (Sym sym) . KeyField =<< getTypeSymbol (typeof e)
                collect $ ConsField exprType i (typeof e)
            _ -> fail "invalid field access"
        collectExpr e

    S.Field _ e symbol@(SymResolved _ _ _) -> do
        ObjField i  <- look (Sym $ Symbol.sym symbol) . KeyField =<< getTypeSymbol (typeof e)
        collect $ ConsField exprType i (typeof e)
        collectExpr e

    S.Match _ e p -> do
        collectPattern p (typeof e)
        collectExpr e
        collectDefault exprType Bool

    S.RecordAccess _ e -> do
        modify $ \s -> s {
            collected = Map.insert (ConsRecordAccess exprType $ typeof e) (curPos s) (collected s)
            }
        collectExpr e

    S.Range _ me me1 me2 -> do
        when (isJust me) $ collectExpr (fromJust me)

        when (isJust me1 && isJust me2) $ do
            collectEq (typeof $ fromJust me1) (typeof $ fromJust me2)
        when (isJust me1) $ do
            collect $ ConsBase (Range $ typeof $ fromJust me1) exprType
            collectDefault (Range $ typeof $ fromJust me1) exprType
            collectExpr (fromJust me1)
        when (isJust me2) $ do
            collect $ ConsBase (Range $ typeof $ fromJust me2) exprType
            collectDefault (Range $ typeof $ fromJust me2) exprType
            collectExpr (fromJust me2)
        when (isNothing me1 && isNothing me2) $ do
            collectDefault (Range I64) exprType

--    S.Array _ es -> do
--        forM_ es $ \e -> do 
--            collectElem exprType (typeof e)
--            collectEq (typeof e) (typeof $ head es)
--
--        collectDefault exprType $ Array (length es) (typeof $ head es)
--
--        mapM_ collectExpr es

    _ -> error (show expr)
