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
        , imports   :: Map.Map ModuleName SymTab
        , curPos    :: TextPos
        }

initCollectState imp = CollectState
    { symTab = SymTab.initSymTab
    , curRetty = Void
    , collected = []
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
                let typedef = T.Typedef (Sym sym)
                forM_ (zip xs [0..]) $ \((s, t), i) -> define s (KeyMember typedef) (ObjMember i)
                define sym KeyType $ ObjType $ T.Tuple (map snd xs)

    forM funcdefs $ \(S.FuncDef pos sym params retty _) -> collectPos pos $
        define sym (KeyFunc $ map paramType params) (ObjFunc retty)

    forM externdefs $ \(S.Extern pos name sym params retty) -> collectPos pos $
        define sym (KeyFunc $ map paramType params) (ObjFunc retty)

    mapM_ collectStmt (astStmts ast)


collectStmt :: BoM CollectState m => Stmt -> m ()
collectStmt stmt = collectPos stmt $ case stmt of
    FuncDef _ sym params retty blk -> do
        oldRetty <- gets curRetty
        modify $ \s -> s { curRetty = retty }
        pushSymTab
        forM_ params $ \(Param _ s t) ->
            define s KeyVar (ObjVar t)
        collectStmt blk
        popSymTab
        modify $ \s -> s { curRetty = oldRetty }
    
    Extern _ name sym params retty ->
        return () -- already defined

    S.Typedef _ _ _ -> return ()

    Block stmts -> do
        pushSymTab
        mapM_ collectStmt stmts
        popSymTab

    Return _ (Just expr) -> do
        rt <- gets curRetty
        assert (rt /= Void) "Cannot return expression in void function."
        collect (typeOf expr) rt
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

    CallStmt p sym exprs -> do
        mapM_ collectExpr exprs
        -- TODO resm <- lookm (Sym sym) (KeyFunc $ map typeOf exprs)

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

    AppendElem _ app expr -> do
        tm <- collectAppend app
        case tm of
            Just (T.Table [te]) -> collect te (typeOf expr)
            _                   -> return ()
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
    CondExpr expr -> collectExpr expr


collectExpr :: BoM CollectState m => Expr -> m ()
collectExpr (AExpr typ expr) = collectPos expr $ case expr of
    Conv p t [e] -> do
        collect typ t
        collectExpr e

    Call p sym [] -> do
        ObjFunc rt <- look sym (KeyFunc [])
        collect typ rt
    
    Call p sym es -> do
        kos <- lookSym sym
        case kos of
            -- no definitions 
            []                         -> fail $ show sym ++ " undefined"
            -- one definition
            [(KeyFunc ts, ObjFunc rt)] -> do
                assert (length ts == length es) "Invalid arguments"
                zipWithM_  collect ts (map typeOf es)
                collect typ rt

            -- several definitions, return type matches one
            _ | length (elemIndices (ObjFunc typ) (map snd kos)) == 1 -> do
                let [idx] = elemIndices (ObjFunc typ) (map snd kos)
                let (KeyFunc ts, ObjFunc _) = kos !! idx
                assert (length ts == length es) "Invalid arguments"
                zipWithM_  collect ts (map typeOf es)

            -- several definitions, arg types match one
            _ | length (elemIndices (KeyFunc $ map typeOf es) (map fst kos)) == 1 -> do
                let Just (ObjFunc rt) = lookup (KeyFunc $ map typeOf es) kos
                collect typ rt

            -- do nothing
            _ -> return ()
                
        mapM_ collectExpr es

    Ident p sym -> do
        ObjVar t <- look sym KeyVar
        collect t typ

    Infix p op e1 e2 -> do
        case op of
            _ | op `elem` [S.Plus, S.Minus, S.Times, S.Divide, S.Modulo] -> collect typ (typeOf e1)
            _ -> return ()
                    
        collect (typeOf e1) (typeOf e2)
        collectExpr e1
        collectExpr e2

    S.Char p c -> return ()
    S.Int p c -> return ()

    S.Prefix p op e -> do
        collect typ (typeOf e)
        collectExpr e

    S.Copy p e -> do
        collect typ (typeOf e)
        collectExpr e

    S.Len p e -> collectExpr e

    S.Bool p b -> return ()
    
    S.Subscript p e1 e2 -> do
        case typeOf e1 of
            Type x      -> return ()
            T.Table [t] -> collect typ t
            _           -> error $ show (typeOf e1)
        collectExpr e1
        collectExpr e2

    S.String p s -> return ()

    S.Tuple p es -> do
        case typ of
            T.Tuple ts -> zipWithM_ collect ts (map typeOf es)
            _          -> return ()
        mapM_ collectExpr es

    Member p e sym -> do
        case typeOf e of
            Type x                -> return ()
            t@(T.Typedef (Sym s)) -> do
                ObjMember i <- look (Sym sym) (KeyMember t)
                ObjType bt <- look (Sym s) KeyType
                case bt of
                    T.Tuple ts -> collect typ (ts !! i)

        collectExpr e

    S.Float p f -> return ()

        

    _ -> fail ("collect: " ++ show expr)
