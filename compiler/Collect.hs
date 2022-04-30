{-# LANGUAGE FlexibleContexts #-}
module Collect where

import Data.Maybe
import Data.List
import qualified Data.Map as Map

import AST as S
import Type as T
import Monad
import Error
import Control.Monad.State
import qualified SymTab


-- constraints obtained from sub-expressions must be to the left
data Constraint
    = Constraint TextPos Type Type
    deriving (Show, Eq)

instance TextPosition Constraint where
    textPos (Constraint pos _ _) = pos

type SymTab = SymTab.SymTab String SymKey Object

data SymKey
    = KeyVar
    | KeyType
    | KeyFunc [Type]
    | KeyMember Type
    deriving (Show, Eq, Ord)

data Object
    = ObjVar Type
    | ObjType Type
    | ObjFunc Type
    | ObjMember Int
    deriving (Show, Eq)

data CollectState
    = CollectState
        { symTab    :: SymTab
        , curRetty  :: Type
        , collected :: [Constraint]
        , defaults  :: [Constraint]
        , imports   :: Map.Map FilePath SymTab
        , curPos    :: TextPos
        }

initCollectState imp = CollectState
    { symTab = SymTab.initSymTab
    , curRetty = Void
    , collected = []
    , defaults = []
    , imports = imp
    , curPos = TextPos 0 0 0 0
    }


collectPos :: (BoM CollectState m, TextPosition t) => t -> m a -> m a
collectPos t m = withPos t $ do
    old <- gets curPos
    modify $ \s -> s { curPos = (textPos t) }
    r <- m
    modify $ \s -> s { curPos = old }
    return r


collect :: BoM CollectState m => Type -> Type -> m ()
collect t1 t2 = do
    modify $ \s -> s { collected = (Constraint (curPos s) t1 t2) : (collected s) }

collectDefault :: BoM CollectState m => Type -> Type -> m ()
collectDefault t1 t2 = do
    modify $ \s -> s { defaults = (Constraint (curPos s) t1 t2) : (defaults s) }

typeOf :: S.Expr -> T.Type
typeOf (S.AExpr t _) = t


look :: BoM CollectState m => Symbol -> SymKey -> m Object
look symbol key = do
    rm <- lookm symbol key
    assert (isJust rm) $ show symbol ++ " " ++ show key ++ " undefined."
    return (fromJust rm)


lookm :: BoM CollectState m => Symbol -> SymKey -> m (Maybe Object)
lookm symbol key = case symbol of
    Sym sym -> do
        lm <- SymTab.lookup sym key <$> gets symTab
        case lm of
            Just _ -> return lm
            Nothing -> do
                ls <- catMaybes . map (SymTab.lookup sym key) . Map.elems <$> gets imports
                case ls of
                    [] -> return Nothing
                    [o] -> return (Just o)
                    _ -> fail $ show symbol ++ " is ambiguous"


lookSym :: BoM CollectState m => Symbol -> m [(SymKey, Object)]
lookSym symbol = case symbol of
    Sym sym -> do
        ls <- SymTab.lookupSym sym <$> gets symTab
        lss <- concat . map (SymTab.lookupSym sym) . Map.elems <$> gets imports
        return (ls ++ lss)

define :: BoM CollectState m => String -> SymKey -> Object -> m ()
define sym key obj = do
    resm <- SymTab.lookupHead sym key <$> gets symTab
    when (isJust resm) $ fail $ sym ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert sym key obj (symTab s) }


pushSymTab :: BoM CollectState m => m ()
pushSymTab = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: BoM CollectState m => m ()
popSymTab = do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


baseTypeOf :: BoM CollectState m => Type -> m Type
baseTypeOf typ = case typ of
    T.Typedef sym -> do ObjType t <- look sym KeyType; baseTypeOf t
    _             -> return typ


collectAST :: BoM CollectState m => AST -> m ()
collectAST ast = do
    let typedefs = [ stmt | stmt@(S.Typedef _ _ _) <- astStmts ast ]
    let funcdefs = [ stmt | stmt@(S.FuncDef _ _ _ _ _) <- astStmts ast ]
    let externdefs = [ stmt | stmt@(S.Extern _ _ _ _ _) <- astStmts ast ]

    forM typedefs $ \(S.Typedef pos sym annoTyp) ->
        collectPos pos $ case annoTyp of
            AnnoType t   ->
                define sym KeyType (ObjType t)

            AnnoTuple xs   -> do
                let ts = map snd xs
                let typedef = T.Typedef (Sym sym)
                forM_ (zip xs [0..]) $ \((s, t), i) -> define s (KeyMember typedef) (ObjMember i)
                define sym KeyType $ ObjType $ T.Tuple (map snd xs)
                define sym (KeyFunc ts) (ObjFunc typedef) 

    forM funcdefs $ \(S.FuncDef pos sym params (Just retty) _) -> collectPos pos $
        define sym (KeyFunc $ map paramType params) (ObjFunc retty)

    forM externdefs $ \(S.Extern pos name sym params retty) -> collectPos pos $
        define sym (KeyFunc $ map paramType params) (ObjFunc retty)

    mapM_ collectStmt (astStmts ast)


collectStmt :: BoM CollectState m => Stmt -> m ()
collectStmt stmt = collectPos stmt $ case stmt of
    FuncDef _ sym params (Just retty) blk -> do
        oldRetty <- gets curRetty
        modify $ \s -> s { curRetty = retty }
        pushSymTab
        forM_ params $ \(Param _ s t) ->
            define s KeyVar (ObjVar t)
        collectStmt blk
        popSymTab
        modify $ \s -> s { curRetty = oldRetty }
        collectDefault retty T.Void
    
    Extern _ name sym params retty ->
        return () -- already defined

    S.Typedef _ _ _ -> return ()

    Block stmts -> do
        pushSymTab
        mapM_ collectStmt stmts
        popSymTab

    Return _ (Just expr) -> do
        collect (typeOf expr) =<< gets curRetty
        collectExpr expr

    If _ cond blk melse -> do
        collectCondition cond
        collectStmt blk
        maybe (return ()) collectStmt melse

    Assign _ pattern expr -> do
        collectPattern pattern (typeOf expr)
        collectExpr expr

    Set _ index expr -> do
        typm <- collectIndex index
        when (isJust typm) $ collect (fromJust typm) (typeOf expr)
        collectExpr expr

    AppendStmt app -> void (collectAppend app)

    While _ cond blk -> do
        collectCondition cond
        collectStmt blk

    CallStmt p sym es -> do
        kos <- lookSym (Sym sym)
        case kos of
            -- no definitions 
            []                         -> fail $ show sym ++ " undefined"
            -- one definition
            [(KeyFunc ts, ObjFunc rt)] -> do
                assert (length ts == length es) "Invalid arguments"
                zipWithM_  collect ts (map typeOf es)

            -- do nothing
            _ -> return ()

        mapM_ collectExpr es

    Print p exprs -> do
        mapM_ collectExpr exprs



    _ -> fail (show stmt)


-- return type of append result
collectAppend :: BoM CollectState m => Append -> m (Maybe Type)
collectAppend append = collectPos append $ case append of
    AppendTable _ app expr -> do
        tm <- collectAppend app
        when (isJust tm) $ collect (fromJust tm) (typeOf expr)
        collectExpr expr
        return tm

    AppendIndex index -> collectIndex index


-- returns type of resulting index
collectIndex :: BoM CollectState m => Index -> m (Maybe Type)
collectIndex index = collectPos index $ case index of
    IndIdent _ s -> do
        ObjVar t <- look (Sym s) KeyVar
        return (Just t)

    IndArray _ ind expr -> do
        collectExpr expr
        t <- collectIndex ind
        case t of
            Just (T.Table [te]) -> return (Just te)
            _                   -> return Nothing

    _ -> fail (show index)


collectPattern :: BoM CollectState m => Pattern -> Type -> m ()
collectPattern pattern typ = collectPos pattern $ case pattern of
    PatIdent _ s -> do
        define s KeyVar (ObjVar typ)

    PatLiteral expr -> collect typ (typeOf expr)

    PatGuarded _ pat expr -> do
        collectPattern pat typ
        collectExpr expr

    _ -> fail (show pattern)


collectCondition :: BoM CollectState m => Condition -> m ()
collectCondition cond = case cond of
    CondExpr expr -> do
        collectDefault (typeOf expr) T.Bool
        collectExpr expr


collectExpr :: BoM CollectState m => Expr -> m ()
collectExpr (AExpr exprType expr) = collectPos expr $ case expr of
    Conv p t [e] -> do
        collect exprType t
        collectExpr e

    Call p sym [] -> do
        ObjFunc rt <- look sym (KeyFunc [])
        collect exprType rt
    
    Call p sym es -> do
        kos <- lookSym sym
        case kos of
            -- no definitions 
            []                         -> fail $ show sym ++ " undefined"
            -- one definition
            [(KeyFunc ts, ObjFunc rt)] -> do
                assert (length ts == length es) "Invalid arguments"
                zipWithM_  collect ts (map typeOf es)
                collect exprType rt

            -- several definitions, return type matches one
            _ | length (elemIndices (ObjFunc exprType) (map snd kos)) == 1 -> do
                let [idx] = elemIndices (ObjFunc exprType) (map snd kos)
                let (KeyFunc ts, ObjFunc _) = kos !! idx
                assert (length ts == length es) "Invalid arguments"
                zipWithM_  collect ts (map typeOf es)

            -- several definitions, arg types match one
            _ | length (elemIndices (KeyFunc $ map typeOf es) (map fst kos)) == 1 -> do
                let Just (ObjFunc rt) = lookup (KeyFunc $ map typeOf es) kos
                collect exprType rt

            -- do nothing
            _ -> return ()
                
        mapM_ collectExpr es

    Ident p sym -> do
        ObjVar t <- look sym KeyVar
        collect t exprType

    Infix p op e1 e2 -> do
        case op of
            _ | op `elem` [S.Plus, S.Minus, S.Times, S.Divide, S.Modulo] -> collect exprType (typeOf e1)
            _ | op `elem` [S.LT, S.GT, S.LTEq, S.GTEq, S.EqEq, S.NotEq]  -> collectDefault exprType T.Bool
            _ | op `elem` [S.AndAnd, S.OrOr]                             -> collect exprType (typeOf e1)
            _ -> return ()
                    
        collect (typeOf e1) (typeOf e2)
        collectExpr e1
        collectExpr e2

    S.Char p c -> collectDefault exprType T.Char
    S.Int p c -> collectDefault exprType I64

    S.Prefix p op e -> do
        collect exprType (typeOf e)
        collectExpr e

    S.Copy p e -> do
        collect exprType (typeOf e)
        collectExpr e

    S.Len p e -> do
        collectDefault exprType I64
        collectExpr e

    S.Bool p b -> collectDefault exprType T.Bool
    
    S.Subscript p e1 e2 -> do
        case typeOf e1 of
            Type x      -> return ()
            T.Table [t] -> collect exprType t
            _           -> error $ show (typeOf e1)
        collectExpr e1
        collectExpr e2

    S.String p s -> collectDefault exprType (T.Table [T.Char])

    S.Tuple p es -> do
        base <- baseTypeOf exprType
        case base of
            T.Tuple ts -> zipWithM_ collect ts (map typeOf es)
            T.Type x   -> return ()
        mapM_ collectExpr es

    Member p e sym -> do
        case typeOf e of
            Type x            -> return ()
            T.Typedef (Sym s) -> do
                ObjMember i  <- look (Sym sym) $ KeyMember (typeOf e)
                ObjType base <- look (Sym s) KeyType
                case base of
                    T.Tuple ts -> collect exprType (ts !! i)

        collectExpr e

    S.Float p f -> collectDefault exprType F64

    S.AExpr _ _ -> fail "what"

    _ -> fail ("collect: " ++ show expr)
