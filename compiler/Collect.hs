{-# LANGUAGE FlexibleContexts #-}
module Collect where

import Data.Maybe
import Data.List
import qualified Data.Map as Map

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
        , collected :: [(TextPos, Constraint)]
        , defaults  :: [(TextPos, Constraint)]
        , imports   :: Map.Map FilePath SymTab
        , curPos    :: TextPos
        , typeSupply :: Int
        , modName   :: String
        }

initCollectState imp mod = CollectState
    { symTab     = SymTab.initSymTab
    , curRetty   = Void
    , collected  = []
    , defaults   = []
    , imports    = imp
    , curPos     = TextPos "" 0 0
    , typeSupply = 0
    , modName    = mod
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
    modify $ \s -> s { collected = (curPos s, ConsAdtMem t i j agg) : (collected s) }

collectField :: BoM CollectState m => Type -> Int -> Type -> m () 
collectField t i agg =
    modify $ \s -> s { collected = (curPos s, ConsField t i agg) : (collected s) }

collect :: BoM CollectState m => Constraint -> m ()
collect constraint = do
    modify $ \s -> s { collected = (curPos s, constraint) : (collected s) }

collectBase :: BoM CollectState m => Type -> Type -> m ()
collectBase t1 t2 = do
    modify $ \s -> s { collected = (curPos s, ConsBase t1 t2) : (collected s) }

collectElem :: BoM CollectState m => Type -> Type -> m ()
collectElem t1 t2 = do
    modify $ \s -> s { collected = (curPos s, ConsElem t1 t2) : (collected s) }

collectSubscript :: BoM CollectState m => Type -> Type -> m ()
collectSubscript t1 t2 = do
    modify $ \s -> s { collected = (curPos s, ConsSubscript t1 t2) : (collected s) }

collectEq :: BoM CollectState m => Type -> Type -> m ()
collectEq t1 t2 = do
    modify $ \s -> s { collected = (curPos s, ConsEq t1 t2) : (collected s) }

collectDefault :: BoM CollectState m => Type -> Type -> m ()
collectDefault t1 t2 = do
    modify $ \s -> s { defaults = (curPos s, ConsEq t1 t2) : (defaults s) }


typeOf :: S.Expr -> Type
typeOf (S.AExpr t _) = t


look :: BoM CollectState m => Symbol -> SymKey -> m Object
look symbol key = do
    rm <- lookm symbol key
    assert (isJust rm) $ show symbol ++ " " ++ show key ++ " undefined."
    return (fromJust rm)


lookm :: BoM CollectState m => Symbol -> SymKey -> m (Maybe Object)
lookm symbol key = do
    imports <- gets $ Map.elems . imports
    symTab <- gets symTab
    return $ lookupSymKey symbol key symTab imports


lookSym :: BoM CollectState m => Symbol -> m [(SymKey, Object)]
lookSym symbol = do
    symTab <- gets symTab
    imports <- gets $ Map.elems . imports
    return $ lookupSym symbol symTab imports



define :: BoM CollectState m => Symbol -> SymKey -> Object -> m ()
define symbol key obj = do
    resm <- SymTab.lookupHead symbol key <$> gets symTab
    assert (isNothing resm) $ show symbol ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert symbol key obj (symTab s) }


collectCExterns :: BoM CollectState m => [Extern] -> m ()
collectCExterns externs = do
    forM_ externs $ \extern -> case extern of
        ExtVar sym (S.AnnoType typ) -> define (SymQualified "c" sym) KeyVar (ObjVar typ)
        ExtFunc sym argTypes retty  -> define (SymQualified "c" sym) (KeyFunc [] argTypes retty) ObjFunc
        ExtConstInt sym n           -> define (SymQualified "c" sym) KeyVar (ObjVar I64)
        ExtTypeDef sym typ          -> define (SymQualified "c" sym) KeyType (ObjType typ)

collectAST :: BoM CollectState m => ResolvedAst -> m ()
collectAST ast = do
    forM (typeDefs ast) $ collectTypedef

    forM (funcDefs ast) $
        \(S.FuncDef pos params symbol args retty _) -> collectPos pos $ do
            define (Sym $ sym symbol) (KeyFunc (map S.paramType params) (map S.paramType args) retty) ObjFunc

    mapM_ collectStmt (funcDefs ast)
    where
        isTypedef :: S.Stmt -> Bool
        isTypedef (S.Typedef _ _ _) = True
        isTypedef _                 = False

        isFuncdef :: S.Stmt -> Bool
        isFuncdef (S.FuncDef _ _ _ _ _ _) = True
        isFuncdef _                       = False


collectTypedef :: BoM CollectState m => S.Stmt -> m ()
collectTypedef (S.Typedef pos symbol annoTyp) = collectPos pos $ case annoTyp of
    S.AnnoType (Tuple ts) -> do
        let typedef = Typedef symbol
        define symbol KeyType (ObjType $ Tuple ts)
        define symbol (KeyFunc [] ts typedef) ObjFunc
        
    S.AnnoType t   -> do
        let typedef = Typedef symbol
        define symbol KeyType (ObjType t)
        define symbol (KeyFunc [] [t] typedef) ObjFunc

    S.AnnoTuple xs   -> do
        let typedef = Typedef symbol
        let ts = map snd xs
        forM_ (zip xs [0..]) $ \((s, t), i) -> define (Sym s) (KeyField typedef) (ObjField i)
        define symbol KeyType $ ObjType $ Tuple (map snd xs)
        define symbol (KeyFunc [] ts typedef) ObjFunc
        define symbol (KeyFunc [] [] typedef) ObjFunc

    S.AnnoADT xs -> do
        let typedef = Typedef symbol
        fs <- forM (zip xs [0..]) $ \(x, i) -> case x of
            S.ADTFieldMember s ts -> do
                define s KeyAdtField (ObjField i)
                define s (KeyFunc [] ts typedef) ObjFunc
                return (FieldCtor ts)

            S.ADTFieldType t -> return (FieldType t)

            S.ADTFieldNull -> return FieldNull

        define symbol KeyType $ ObjType (ADT fs)



collectStmt :: BoM CollectState m => S.Stmt -> m ()
collectStmt stmt = collectPos stmt $ case stmt of
    S.Typedef _ _ _ -> collectTypedef stmt
    S.Print p exprs -> mapM_ collectExpr exprs
    S.Typedef _ _ _ -> return ()
    S.Block stmts -> mapM_ collectStmt stmts
    S.ExprStmt e -> collectExpr e

    S.FuncDef _ params sym args retty blk -> do
        oldRetty <- gets curRetty
        modify $ \s -> s { curRetty = retty }

        forM params $ \(S.Param _ symbol t) ->
            define symbol KeyVar (ObjVar t)

        forM_ args $ \(S.Param _ symbol t) ->
            define symbol KeyVar (ObjVar t)
        collectStmt blk
        modify $ \s -> s { curRetty = oldRetty }
        collectDefault retty Void

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

    S.Data p symbol typ -> do
        define symbol KeyVar (ObjVar typ)
        

    _ -> error (show stmt)


-- collectPattern pattern <with this type of expression trying to match>
collectPattern :: BoM CollectState m => S.Pattern -> Type -> m ()
collectPattern pattern typ = collectPos pattern $ case pattern of
    S.PatIgnore pos -> return ()
    S.PatIdent _ symbol -> do
        define symbol KeyVar (ObjVar typ)

    S.PatLiteral expr -> collectEq typ (typeOf expr)

    S.PatGuarded _ pat expr -> do
        collectPattern pat typ
        collectBase (typeOf expr) Bool
        collectExpr expr

    S.PatField _ symbol pats -> do
        resm <- lookm symbol KeyAdtField
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
collectCall exprType ps symbol es = do
    kos <- lookSym symbol
    let ks = [ k | (k@(KeyFunc _ _ _), ObjFunc) <- kos ]

    let ksSameRetty   = [ k | k@(KeyFunc _ _ rt) <- ks, rt == exprType ]
    let ksSameArgs    = [ k | k@(KeyFunc _ xs _) <- ks, xs == map typeOf es ]
    let ksSameArgsLen = [ k | k@(KeyFunc _ xs _) <- ks, length xs == length es ]
    let ksSameParams  = [ k | k@(KeyFunc xs _ _) <- ks, xs == map typeOf ps ]

    let kss = [ks, ksSameRetty, ksSameArgs, ksSameArgsLen, ksSameParams]
    mapM_ collectIfOneDef kss
    collectIfOneDef $ intersectMatches kss
    
    mapM_ collectExpr ps
    mapM_ collectExpr es
    where
        collectIfOneDef :: BoM CollectState m => [SymKey] -> m ()
        collectIfOneDef [KeyFunc xs ys z] = do
            assert (length ys == length es) "Invalid number of arguments"
            assert (length xs == length ps) "Invalid number of parameters"
            collectEq z exprType
            zipWithM_ collectEq xs (map typeOf ps)
            zipWithM_ collectEq ys (map typeOf es)
        collectIfOneDef _ = return ()

        intersectMatches :: [[SymKey]] -> [SymKey]
        intersectMatches []       = []
        intersectMatches (ks:kss) = case intersectMatches kss of
            []  -> ks
            ks2 -> intersect ks ks2


collectExpr :: BoM CollectState m => S.Expr -> m ()
collectExpr (S.AExpr exprType expr) = collectPos expr $ case expr of
    S.Call _ ps s es -> collectCall exprType ps s es
    S.Conv _ t [e]   -> collectEq exprType t >> collectExpr e
    S.Prefix _ op e  -> collectEq exprType (typeOf e) >> collectExpr e
    S.Int _ c        -> collectDefault exprType I64
    S.Len _ e        -> collectDefault exprType I64 >> collectExpr e
    S.Float _ f      -> collectDefault exprType F64

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
        collectBase exprType String
        collectDefault exprType String

    S.UnsafePtr _ e -> do
        collectEq exprType (UnsafePtr (typeOf e))
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

    S.Array _ [] -> return ()
    S.Array _ es -> do
        forM_ es        $ \e -> collectElem (typeOf e) exprType
        forM_ (tail es) $ \e -> collectEq (typeOf e) (typeOf $ head es)
        collectDefault exprType $ Array (length es) (typeOf $ head es)
        collectBase exprType $ Array (length es) (typeOf $ head es)
        mapM_ collectExpr es

    S.Tuple _ es -> do
        collectBase exprType $ Tuple (map typeOf es)
        collectDefault exprType $ Tuple (map typeOf es)
        mapM_ collectExpr es

    S.Field _ e sym -> do
        case typeOf e of
            Type x         -> return ()
            Typedef symbol -> do
                ObjField i  <- look (Sym sym) $ KeyField (typeOf e)
                collectField exprType i (typeOf e)
        collectExpr e

    S.TupleIndex _ e i -> do
        collectField exprType (fromIntegral i) (typeOf e)
        collectExpr e

    S.Null _ -> return ()

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
