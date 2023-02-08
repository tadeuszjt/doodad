{-# LANGUAGE FlexibleContexts #-}
module Collect where

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import Type
import Monad
import Error
import Control.Monad.State
import qualified SymTab
import Interop
import Symbol
import qualified Resolve
import States

import qualified Debug.Trace

-- constraints obtained from sub-expressions must be to the left
data Constraint
    = ConsEq Type Type
    | ConsBase   Type Type -- both types must have same base
    | ConsElem   Type Type -- t1 is elem type of t2
    | ConsSubscript   Type Type -- t1 is elem type of t2
    | ConsField Type Int Type 
    | ConsAdtMem Type Int Int Type
    deriving (Show, Eq, Ord)

type SymTab = SymTab.SymTab Symbol SymKey Object

data SymKey
    = KeyVar
    | KeyType
    | KeyFunc [Type] [Type] Type
    | KeyField Type
    | KeyAdtField
    deriving (Show, Eq, Ord)

data Object
    = ObjVar Type
    | ObjType Type
    | ObjFunc 
    | ObjField Int
    deriving (Show, Eq)

data CollectState
    = CollectState
        { symTab    :: SymTab
        , curRetty  :: Type
        , collected :: Map.Map Constraint TextPos
        , defaults  :: Map.Map Constraint TextPos
        , curPos    :: TextPos
        , typeSupply :: Int
        }

initCollectState = CollectState
    { symTab     = SymTab.initSymTab
    , curRetty   = Void
    , collected  = Map.empty
    , defaults   = Map.empty
    , curPos     = TextPos "" 0 0
    , typeSupply = 0
    }


genType :: BoM CollectState m => m Type
genType = do
    i <- gets typeSupply
    modify $ \s -> s { typeSupply = i - 1 }
    return $ Type (i - 1)


collectPos :: (BoM CollectState m, TextPosition t) => t -> m a -> m a
collectPos t m = withPos t $ do
    old <- gets curPos
    modify $ \s -> s { curPos = (textPos t) }
    r <- m
    modify $ \s -> s { curPos = old }
    return r

collectAdtField :: BoM CollectState m => Type -> Int -> Int -> Type -> m () 
collectAdtField t i j agg =
    modify $ \s -> s { collected = Map.insert (ConsAdtMem t i j agg) (curPos s) (collected s) }

collectField :: BoM CollectState m => Type -> Int -> Type -> m () 
collectField t i agg =
    modify $ \s -> s { collected = Map.insert (ConsField t i agg) (curPos s) (collected s) }

collect :: BoM CollectState m => Constraint -> m ()
collect constraint = do
    modify $ \s -> s { collected = Map.insert (constraint) (curPos s) (collected s) }

collectBase :: BoM CollectState m => Type -> Type -> m ()
collectBase t1 t2 = do
    modify $ \s -> s { collected = Map.insert (ConsBase t1 t2) (curPos s) (collected s) }

collectElem :: BoM CollectState m => Type -> Type -> m ()
collectElem t1 t2 = do
    modify $ \s -> s { collected = Map.insert (ConsElem t1 t2) (curPos s) (collected s) }

collectSubscript :: BoM CollectState m => Type -> Type -> m ()
collectSubscript t1 t2 = do
    modify $ \s -> s { collected = Map.insert (ConsSubscript t1 t2) (curPos s) (collected s) }

collectEq :: BoM CollectState m => Type -> Type -> m ()
collectEq t1 t2 = do
    modify $ \s -> s { collected = Map.insert (ConsEq t1 t2) (curPos s) (collected s) }

collectDefault :: BoM CollectState m => Type -> Type -> m ()
collectDefault t1 t2 = do
    modify $ \s -> s { defaults = Map.insert (ConsEq t1 t2) (curPos s) (defaults s) }


typeOf :: S.Expr -> Type
typeOf (S.AExpr t _) = t


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


collectAST :: BoM CollectState m => ResolvedAst -> m ()
collectAST ast = do
    forM (Map.toList $ typeImports ast) $ \(symbol, typ) ->
        collectTypedef symbol typ

    forM (Map.toList $ typeDefs ast) $ \(symbol, t) -> do
        collectTypedef symbol t

    forM (Map.toList $ ctorImports ast) $ \(symbol, (t, i)) -> do
        collectCtorDef symbol (t, i)

    forM (Map.toList $ ctorDefs ast) $ \(symbol, (t, i)) -> do
        collectCtorDef symbol (t, i)

    forM (Map.toList $ funcImports ast) $ \(symbol, key@(ps, _, as, rt)) -> 
        define symbol (KeyFunc ps as rt) ObjFunc

    forM (Map.toList $ funcDefs ast) $ \(symbol, body) ->
        define symbol (KeyFunc (map S.paramType $ funcParams body) (map S.paramType $ funcArgs body) (funcRetty body)) ObjFunc

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) ->
        collectFuncDef symbol body


collectFuncDef :: BoM CollectState m => Symbol -> FuncBody -> m ()
collectFuncDef symbol body = do
    oldRetty <- gets curRetty
    modify $ \s -> s { curRetty = funcRetty body }

    forM (funcParams body) $ \(S.Param _ symbol t) ->
        define symbol KeyVar (ObjVar t)

    forM_ (funcArgs body) $ \(S.Param _ symbol t) ->
        define symbol KeyVar (ObjVar t)
    mapM_ collectStmt (funcStmts body)
    modify $ \s -> s { curRetty = oldRetty }
    collectDefault (funcRetty body) Void


collectCtorDef :: BoM CollectState m => Symbol -> (Type, Int) -> m ()
collectCtorDef symbol (Typedef s@(SymResolved _ _ _), i) = withErrorPrefix "collectCtorDef" $ do
    ObjType ot <- look s KeyType -- check
    define symbol KeyAdtField (ObjField i)
    case ot of
        Tuple ts -> define (Sym $ sym symbol) (KeyField $ Typedef s) (ObjField i)
        Enum     -> define symbol (KeyFunc [] [] $ Typedef s) ObjFunc
        ADT fs   -> case fs !! i of
            FieldCtor ts -> define symbol (KeyFunc [] ts $ Typedef s) ObjFunc
            _            -> return ()
            
        _ -> return ()


collectTypedef :: BoM CollectState m => Symbol -> Type -> m ()
collectTypedef symbol typ = do
    let typedef = Typedef symbol
    define symbol KeyType (ObjType typ)
    case typ of
        Tuple ts -> define symbol (KeyFunc [] ts typedef) ObjFunc
        t        -> define symbol (KeyFunc [] [t] typedef) ObjFunc


collectStmt :: BoM CollectState m => S.Stmt -> m ()
collectStmt stmt = collectPos stmt $ case stmt of
    --S.Typedef _ symbol anno -> collectTypedef symbol anno
    S.Print p exprs -> mapM_ collectExpr exprs
    S.Typedef _ _ _ -> return ()
    S.Block stmts -> mapM_ collectStmt stmts
    S.ExprStmt e -> collectExpr e

    S.Return _ mexpr -> do
        retty <- gets curRetty
        case mexpr of
            Nothing -> collectEq Void retty
            Just expr -> do
                collectEq (typeOf expr) retty
                collectExpr expr

    S.If _ expr blk melse -> do
        collectBase Bool (typeOf expr)
        collectExpr expr
        collectStmt blk
        maybe (return ()) collectStmt melse

    S.Assign _ pattern expr -> do
        collectPattern pattern (typeOf expr)
        collectExpr expr

    S.Set _ expr1 expr2 -> do
        collectEq (typeOf expr1) (typeOf expr2)
        collectExpr expr1
        collectExpr expr2

    S.While _ expr blk -> do
        collectExpr expr
        collectBase Bool (typeOf expr)
        collectStmt blk

    S.Switch p expr cases -> do
        collectExpr expr
        forM_ cases $ \(pat, stmt) -> do
            collectPattern pat (typeOf expr)
            collectStmt stmt

    S.For p expr mpat blk -> do
        when (isJust mpat) $ do
            gt <- genType
            collectElem gt (typeOf expr)
            collectPattern (fromJust mpat) gt

        collectExpr expr
        collectStmt blk

    S.Data p symbol typ mexpr -> do
        define symbol KeyVar (ObjVar typ)
        maybe (return ()) (collectEq typ . typeOf) mexpr
        maybe (return ()) collectExpr mexpr
        
    _ -> error (show stmt)


-- collectPattern pattern <with this type of expression trying to match>
collectPattern :: BoM CollectState m => S.Pattern -> Type -> m ()
collectPattern pattern typ = collectPos pattern $ case pattern of
    S.PatIgnore pos -> return ()
    S.PatIdent _ symbol -> do
        define symbol KeyVar (ObjVar typ)

    S.PatLiteral expr -> do 
        collectEq typ (typeOf expr)
        collectExpr expr

    S.PatGuarded _ pat expr -> do
        collectPattern pat typ
        collectBase (typeOf expr) Bool
        collectExpr expr

    S.PatField _ symbol pats -> do
        resm <- SymTab.lookup symbol KeyAdtField <$> gets symTab
        case resm of
            Just (ObjField i) -> do
                gts <- replicateM (length pats) genType
                zipWithM_ collectPattern pats gts
                forM_ (zip gts [0..]) $ \(t, j) -> collectAdtField t i j typ
            Nothing -> do
                ObjType t <- look symbol KeyType
                assert (length pats == 1) "One pattern needed for type field"
                collectPattern (head pats) t

    S.PatTypeField _ t pat -> do
        collectPattern pat t

    S.PatTuple _ pats -> do
        gts <- replicateM (length pats) genType
        collectDefault typ (Tuple gts)
        collectBase typ (Tuple gts)
        zipWithM_ collectPattern pats gts

    S.PatArray _ pats -> do
        gt <- genType
        mapM_ (\p -> collectPattern p gt) pats
        collectElem gt typ

    S.PatAnnotated pat t -> do
        collectEq t typ
        collectPattern pat typ

    S.PatNull _ -> return ()
        
    _ -> error $ show pattern


collectCall :: BoM CollectState m => Type -> [S.Expr] -> Symbol -> [S.Expr] -> m ()
collectCall exprType rs symbol es = do -- can be resolved or sym
    kos <- case symbol of
        SymQualified mod sym -> do
            let f = (\k v -> Symbol.sym k == sym && Symbol.mod k == mod)
            maps <- Map.elems . Map.filterWithKey f . head <$> gets symTab
            return $ Map.toList $ Map.unions maps
        Sym sym -> do
            let f = (\k v -> Symbol.sym k == sym)
            maps <- Map.elems . Map.filterWithKey f . head <$> gets symTab
            return $ Map.toList $ Map.unions maps
        SymResolved _ _ _ -> SymTab.lookupSym symbol <$> gets symTab

    let ks = filter keyCouldMatch $ map fst kos
    collectIfUnifiedType (map (\(KeyFunc ps _ _) -> ps) ks) (map typeOf rs)
    collectIfUnifiedType (map (\(KeyFunc _ as _) -> as) ks) (map typeOf es)
    collectIfUnifiedType (map (\(KeyFunc _ _ r) -> [r]) ks) [exprType]
    collectIfOneDef ks
    
    mapM_ collectExpr rs
    mapM_ collectExpr es
    where
        keyCouldMatch :: SymKey -> Bool
        keyCouldMatch (KeyFunc ps as r)
            | length ps /= length rs || length as /= length es = False
            | otherwise = all (== True) $ zipWith typesCouldMatch (ps ++ as) $ map typeOf (rs ++ es)
            where
                typesCouldMatch :: Type -> Type -> Bool
                typesCouldMatch (Type _) _ = True
                typesCouldMatch _ (Type _) = True
                typesCouldMatch a b        = a == b
        keyCouldMatch _ = False

        collectIfUnifiedType :: BoM CollectState m => [[Type]] -> [Type] -> m ()
        collectIfUnifiedType [] types = return () 
        collectIfUnifiedType typess types = do 
            assert (all (== length types) $ map length typess) "lengths do not match"
            forM_ [0..length types - 1] $ \i -> do 
                let typesn = map (!! i) typess 
                when (all (== head typesn) typesn) $ do 
                    collectEq (head typesn) (types !! i)

        collectIfOneDef :: BoM CollectState m => [SymKey] -> m ()
        collectIfOneDef [KeyFunc xs ys z] = do
            assert (length ys == length es) "Invalid number of arguments"
            assert (length xs == length rs) "Invalid number of parameters"
            collectEq z exprType
            zipWithM_ collectEq xs (map typeOf rs)
            zipWithM_ collectEq ys (map typeOf es)
        collectIfOneDef _ = return ()


collectExpr :: BoM CollectState m => S.Expr -> m ()
collectExpr (S.AExpr exprType expr) = collectPos expr $ case expr of
    S.Call _ ps s es -> collectCall exprType ps s es
    S.Prefix _ op e  -> collectEq exprType (typeOf e) >> collectExpr e
    S.Int _ c        -> collectDefault exprType I64
    S.Len _ e        -> collectDefault exprType I64 >> collectExpr e
    S.Float _ f      -> collectDefault exprType F64
    S.Null _         -> return ()

    S.Conv _ t [e]   -> do 
        collectExpr e
        collectEq exprType t

    S.Push _ e es        -> do
        collectDefault exprType I64
        forM es $ \a -> collectElem (typeOf a) (typeOf e)
        collectExpr e
        mapM_ collectExpr es

    S.Pop _ e es        -> do
        assert (es == []) "pop cannot have arguments"
        collectElem exprType (typeOf e)
        collectExpr e

    S.Clear _ e        -> do
        collectExpr e
        collectEq exprType Void

    S.Delete _ expr1 expr2 -> do
        collectEq exprType Void
        collectExpr expr1
        collectExpr expr2

    S.Char _ c       -> do
        collectBase exprType Char
        collectDefault exprType Char

    S.Bool _ b       -> do
        collectBase exprType Bool
        collectDefault exprType Bool

    S.String _ s     -> do
        collectDefault exprType String

    S.UnsafePtr _ e -> do
        collectEq exprType UnsafePtr
        collectExpr e

    S.Ident _ symbol -> do
        ObjVar t <- look symbol KeyVar
        collectEq t exprType

    S.Infix _ op e1 e2 -> do
        case op of
            _ | op `elem` [S.Plus, S.Minus, S.Times, S.Divide, S.Modulo] -> do
                collectEq exprType (typeOf e1)
            _ | op `elem` [S.LT, S.GT, S.LTEq, S.GTEq, S.EqEq, S.NotEq]  -> do
                collectBase exprType Bool
                collectDefault exprType Bool
            _ | op `elem` [S.AndAnd, S.OrOr] -> do
                collectBase exprType Bool
                collectEq exprType (typeOf e1)
            _ -> return ()
                    
        collectEq (typeOf e1) (typeOf e2)
        collectExpr e1
        collectExpr e2

    S.Subscript _ e1 e2 -> do
        collectSubscript exprType (typeOf e1)
        collectExpr e1
        collectExpr e2
        collectDefault (typeOf e2) I64

    S.Initialiser _ [] -> return ()
    S.Initialiser _ es -> do
        forM_ es        $ \e -> collectElem (typeOf e) exprType
        forM_ (tail es) $ \e -> collectEq (typeOf e) (typeOf $ head es)
        collectDefault exprType $ Array (length es) (typeOf $ head es)
        mapM_ collectExpr es

    S.Tuple _ es -> do
        collectBase exprType $ Tuple (map typeOf es)
        collectDefault exprType $ Tuple (map typeOf es)
        mapM_ collectExpr es

    S.Field _ e (Sym sym) -> do
        case typeOf e of
            Type x         -> return ()
            Typedef symbol -> do
                ObjField i  <- look (Sym sym) $ KeyField (typeOf e)
                collectField exprType i (typeOf e)
        collectExpr e

    S.TupleIndex _ e i -> do
        collectField exprType (fromIntegral i) (typeOf e)
        collectExpr e

    S.AExpr _ _ -> fail "what"

    S.ADT _ e -> do
        collectExpr e
        return () -- TODO

    S.Match _ e p -> do
        collectPattern p (typeOf e)
        collectExpr e
        collectDefault exprType Bool

    S.Range _ Nothing me1 me2 -> do
        when (isJust me1 && isJust me2) $ do
            collectEq (typeOf $ fromJust me1) (typeOf $ fromJust me2)
        when (isJust me1) $ do
            collectBase (Range $ typeOf $ fromJust me1) exprType
            collectDefault (Range $ typeOf $ fromJust me1) exprType
            collectExpr (fromJust me1)
        when (isJust me2) $ do
            collectBase (Range $ typeOf $ fromJust me2) exprType
            collectDefault (Range $ typeOf $ fromJust me2) exprType
            collectExpr (fromJust me2)

    S.Range _ (Just e) me1 me2 -> do
        collectBase (Range I64) exprType
        collectDefault (Range I64) exprType
        collectExpr e

        when (isJust me1) $ do
            collectEq (typeOf $ fromJust me1) I64
            collectExpr (fromJust me1)

        when (isJust me2) $ do
            collectEq (typeOf $ fromJust me2) I64
            collectExpr (fromJust me2)

    _ -> error (show expr)
