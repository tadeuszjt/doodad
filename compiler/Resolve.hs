{-# LANGUAGE FlexibleContexts #-}
module Resolve where

import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe
import Data.List

import qualified SymTab
import Type
import AST
import Monad
import Error


class Resolve a where
    resolve :: BoM ResolveState m => a -> m a

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


type SymTab = SymTab.SymTab String SymKey Object

data ResolveState
    = ResolveState
        { symTab    :: SymTab
        , curRetty  :: Type
        , imports   :: Map.Map FilePath SymTab
        }

initResolveState imp = ResolveState
    { symTab     = SymTab.initSymTab
    , curRetty   = Void
    , imports    = imp
    }


look :: BoM ResolveState m => Symbol -> SymKey -> m Object
look symbol key = do
    rm <- lookm symbol key
    assert (isJust rm) $ show symbol ++ " " ++ show key ++ " undefined."
    return (fromJust rm)


lookm :: BoM ResolveState m => Symbol -> SymKey -> m (Maybe Object)
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


lookSym :: BoM ResolveState m => Symbol -> m [(SymKey, Object)]
lookSym symbol = case symbol of
    Sym sym -> do
        ls <- SymTab.lookupSym sym <$> gets symTab
        lss <- concat . map (SymTab.lookupSym sym) . Map.elems <$> gets imports
        return (ls ++ lss)

    SymQualified mod sym -> do
        statem <- Map.lookup mod <$> gets imports
        case statem of
            Nothing -> fail $ mod ++ " not imported"
            Just state -> do
                return $ SymTab.lookupSym sym state
        

define :: BoM ResolveState m => String -> SymKey -> Object -> m ()
define sym key obj = do
    resm <- SymTab.lookupHead sym key <$> gets symTab
    when (isJust resm) $ fail $ sym ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert sym key obj (symTab s) }


pushSymTab :: BoM ResolveState m => m ()
pushSymTab = do
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: BoM ResolveState m => m ()
popSymTab = do
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


resolveAST :: BoM ResolveState m => AST -> m AST
resolveAST ast = do
    let (typedefs, stmts'') = partition isTypedef (astStmts ast)
    let (funcdefs, stmts') = partition isFuncdef stmts''
    let (externdefs, stmts) = partition isExterndef stmts'

    --forM typedefs $ resolveTypedef

    forM funcdefs $ \(FuncDef pos sym params retty _) ->
        define sym (KeyFunc $ map paramType params) (ObjFunc retty)

    forM externdefs $ \(Extern pos name sym params retty) ->
        define sym (KeyFunc $ map paramType params) (ObjFunc retty)

    astStmts <- mapM resolve (astStmts ast)
    return $ ast { astStmts = astStmts }
    where
        isTypedef :: Stmt -> Bool
        isTypedef (AST.Typedef _ _ _) = True
        isTypedef _                 = False

        isFuncdef :: Stmt -> Bool
        isFuncdef (FuncDef _ _ _ _ _) = True
        isFuncdef _                     = False

        isExterndef :: Stmt -> Bool
        isExterndef (Extern _ _ _ _ _)  = True
        isExterndef _                     = False

instance Resolve Stmt where
    resolve stmt = case stmt of
        _ -> fail "stmt"
--
--resolveTypedef :: BoM ResolveState m => Stmt -> m ()
--resolveTypedef (S.Typedef pos sym annoTyp) = case annoTyp of
--    AnnoType t   ->
--        define sym KeyType (ObjType t)
--
--    AnnoTuple xs   -> do
--        let ts = map snd xs
--        let typedef = T.Typedef (Sym sym)
--        forM_ (zip xs [0..]) $ \((s, t), i) -> define s (KeyMember typedef) (ObjMember i)
--        define sym KeyType $ ObjType $ T.Tuple (map snd xs)
--        define sym (KeyFunc ts) (ObjFunc typedef) 
--
--    AnnoADT xs -> do
--        let ts = map snd xs
--        let typedef = T.Typedef (Sym sym)
--        forM_ (zip xs [0..]) $ \((s, t), i) -> do
--            define s (KeyMember typedef) (ObjMember i)
--            define s (KeyFunc [t]) (ObjFunc typedef)
--        define sym KeyType $ ObjType $ T.ADT (map snd xs)
--
--
--
--resolveStmt :: BoM ResolveState m => Stmt -> m ()
--resolveStmt stmt = resolvePos stmt $ case stmt of
--    S.Typedef _ _ _                -> resolveTypedef stmt
--    Print p exprs                  -> mapM_ resolveExpr exprs
--    S.Typedef _ _ _                -> return ()
--    ExternVar p n s t              -> define s KeyVar (ObjVar t)
--    Extern _ name sym params retty -> return () -- already defined
--    Block stmts                    -> pushSymTab >> mapM_ resolveStmt stmts >> popSymTab
--
--    FuncDef _ sym params retty blk -> do
--        oldRetty <- gets curRetty
--        modify $ \s -> s { curRetty = retty }
--        pushSymTab
--        forM_ params $ \(Param _ s t) ->
--            define s KeyVar (ObjVar t)
--        resolveStmt blk
--        popSymTab
--        modify $ \s -> s { curRetty = oldRetty }
--        resolveDefault retty T.Void
--
--    Return _ (Just expr) -> do
--        resolve (typeOf expr) =<< gets curRetty
--        resolveExpr expr
--
--    If _ cond blk melse -> do
--        resolveCondition cond
--        resolveStmt blk
--        maybe (return ()) resolveStmt melse
--
--    Assign _ pattern expr -> do
--        resolvePattern pattern (typeOf expr)
--        resolveExpr expr
--
--    Set _ index expr -> do
--        typm <- resolveIndex index
--        when (isJust typm) $ resolve (fromJust typm) (typeOf expr)
--        resolveExpr expr
--
--    AppendStmt app -> void (resolveAppend app)
--
--    While _ cond blk -> do
--        resolveCondition cond
--        resolveStmt blk
--
--    CallStmt p sym es -> do
--        kos <- lookSym (Sym sym)
--        case kos of
--            -- no definitions 
--            []                         -> fail $ show sym ++ " undefined"
--            -- one definition
--            [(KeyFunc ts, ObjFunc rt)] -> do
--                assert (length ts == length es) "Invalid arguments"
--                zipWithM_  resolve ts (map typeOf es)
--
--            -- do nothing
--            _ -> return ()
--
--        mapM_ resolveExpr es
--
--    Switch p expr cases -> do
--        resolveExpr expr
--        pushSymTab
--        forM_ cases $ \(pat, stmt) -> do
--            resolvePattern pat (typeOf expr)
--            resolveStmt stmt
--        popSymTab
--
--    _ -> fail (show stmt)
--
--
---- return type of append result
--resolveAppend :: BoM ResolveState m => Append -> m (Maybe Type)
--resolveAppend append = resolvePos append $ case append of
--    AppendTable _ app expr -> do
--        tm <- resolveAppend app
--        when (isJust tm) $ resolve (fromJust tm) (typeOf expr)
--        resolveExpr expr
--        return tm
--
--    AppendIndex index -> resolveIndex index
--
--
---- returns type of resulting index
--resolveIndex :: BoM ResolveState m => Index -> m (Maybe Type)
--resolveIndex index = resolvePos index $ case index of
--    IndIdent _ s -> do
--        ObjVar t <- look (Sym s) KeyVar
--        return (Just t)
--
--    IndArray _ ind expr -> do
--        resolveExpr expr
--        t <- resolveIndex ind
--        case t of
--            Just (T.Table [te]) -> return (Just te)
--            _                   -> return Nothing
--
--    _ -> fail (show index)
--
--
--resolvePattern :: BoM ResolveState m => Pattern -> m ()
--resolvePattern pattern = resolvePos pattern $ case pattern of
--    PatIdent _ s -> do
--        define s KeyVar (ObjVar typ)
--
--    PatLiteral expr -> resolve typ (typeOf expr)
--
--    PatGuarded _ pat expr -> do
--        resolvePattern pat typ
--        resolveExpr expr
--
--    PatField _ symbol pat -> do
--        base <- baseTypeOf typ
--        case base of
--            ADT ts -> do
--                ObjMember i <- look symbol (KeyMember typ)
--                resolvePattern pat (ts !! i)
--            _ -> resolvePattern pat =<< genType
--
--    _ -> fail $ "cannot resolve: " ++ show pattern
--
--
--resolveCondition :: BoM ResolveState m => Condition -> m ()
--resolveCondition cond = case cond of
--    CondExpr expr -> do
--        resolveDefault (typeOf expr) T.Bool
--        resolveExpr expr
--
--
--
--resolveExpr :: BoM ResolveState m => Expr -> m ()
--resolveExpr (AExpr exprType expr) = resolvePos expr $ case expr of
--    Call p symbol es -> resolveCallExpr (AExpr exprType expr)
--    Conv p t [e] -> resolve exprType t >> resolveExpr e
--    S.Char p c -> resolveDefault exprType T.Char
--    S.Int p c -> resolveDefault exprType I64
--    S.Prefix p op e -> resolve exprType (typeOf e) >> resolveExpr e
--    S.Copy p e -> resolve exprType (typeOf e) >> resolveExpr e
--    S.Len p e -> resolveDefault exprType I64 >> resolveExpr e
--    S.Bool p b -> resolveDefault exprType T.Bool
--    S.String p s -> resolveDefault exprType (T.Table [T.Char])
--    S.Float p f -> resolveDefault exprType F64
--
--    Ident p sym -> do
--        ObjVar t <- look sym KeyVar
--        resolve t exprType
--
--    Infix p op e1 e2 -> do
--        case op of
--            _ | op `elem` [S.Plus, S.Minus, S.Times, S.Divide, S.Modulo] -> resolve exprType (typeOf e1)
--            _ | op `elem` [S.LT, S.GT, S.LTEq, S.GTEq, S.EqEq, S.NotEq]  -> resolveDefault exprType T.Bool
--            _ | op `elem` [S.AndAnd, S.OrOr]                             -> resolve exprType (typeOf e1)
--            _ -> return ()
--                    
--        resolve (typeOf e1) (typeOf e2)
--        resolveExpr e1
--        resolveExpr e2
--
--    S.Subscript p e1 e2 -> do
--        case typeOf e1 of
--            Type x      -> return ()
--            T.Table [t] -> resolve exprType t
--            _           -> error $ show (typeOf e1)
--        resolveExpr e1
--        resolveExpr e2
--
--
--    S.Tuple p es -> do
--        base <- baseTypeOf exprType
--        case base of
--            T.Tuple ts -> zipWithM_ resolve ts (map typeOf es)
--            T.Type x   -> return ()
--        mapM_ resolveExpr es
--
--    Member p e sym -> do
--        case typeOf e of
--            Type x            -> return ()
--            T.Typedef (Sym s) -> do
--                ObjMember i  <- look (Sym sym) $ KeyMember (typeOf e)
--                ObjType base <- look (Sym s) KeyType
--                case base of
--                    T.Tuple ts -> resolve exprType (ts !! i)
--
--        resolveExpr e
--
--
--    S.AExpr _ _ -> fail "what"
--
--    _ -> fail ("resolve: " ++ show expr)
