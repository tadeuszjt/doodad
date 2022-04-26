{-# LANGUAGE FlexibleContexts #-}
module Collect where

import Data.Maybe

import AST as S
import Type as T
import Monad
import Error
import Control.Monad.State
import qualified SymTab


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
        { symTab :: SymTab
        , curRetty :: Type
        , collected :: [(Type, Type)]
        }

initCollectState = CollectState
    { symTab = SymTab.initSymTab
    , curRetty = Void
    , collected = []
    }


collect :: BoM CollectState m => Type -> Type -> m ()
collect t1 t2 = modify $ \s -> s { collected = (t1, t2) : (collected s) }


typeOf :: S.Expr -> T.Type
typeOf (S.AExpr t _) = t


look :: BoM CollectState m => Symbol -> SymKey -> m Object
look symbol key = do
    rm <- lookm symbol key
    assert (isJust rm) $ show symbol ++ " undefined."
    return (fromJust rm)

lookm :: BoM CollectState m => Symbol -> SymKey -> m (Maybe Object)
lookm symbol key = case symbol of
    Sym sym -> SymTab.lookup sym key <$> gets symTab
        



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
    forM [ stmt | stmt@(S.Typedef _ _ _) <- astStmts ast ] $
        \(S.Typedef pos sym annoTyp) ->
            withPos pos $ define sym KeyType =<< case annoTyp of
                AnnoType t   -> return (ObjType t)

                AnnoTuple xs   -> do
                    let typedef = T.Typedef (Sym sym)
                    forM_ (zip xs [0..]) $ \((s, t), i) -> define s (KeyMember typedef) (ObjMember i)
                    return $ ObjType $ T.Tuple (map snd xs)

                AnnoADT xs -> do
                    let typedef = T.Typedef (Sym sym)
                    forM_ (zip xs [0..]) $ \((s, t), i) -> define s (KeyMember typedef) (ObjMember i)
                    return $ ObjType $ T.ADT (map snd xs)

    forM [ stmt | stmt@(S.FuncDef _ _ _ _ _) <- astStmts ast ] $
        \(S.FuncDef pos sym params retty _) ->
            define sym (KeyFunc $ map paramType params) (ObjFunc retty)

    forM [ stmt | stmt@(S.Extern _ _ _ _ _) <- astStmts ast ] $
        \(S.Extern pos name sym params retty) ->
            define sym (KeyFunc $ map paramType params) (ObjFunc retty)

    mapM_ collectStmt (astStmts ast)


collectStmt :: BoM CollectState m => Stmt -> m ()
collectStmt stmt = withPos stmt $ case stmt of
    FuncDef _ sym params retty blk -> do
        oldRetty <- gets curRetty
        modify $ \s -> s { curRetty = retty }
        pushSymTab
        forM_ params $ \(Param _ s t) ->
            define s KeyVar (ObjVar t)
        collectStmt blk
        popSymTab
        modify $ \s -> s { curRetty = oldRetty }

    Block stmts -> pushSymTab >> mapM_ collectStmt stmts >> popSymTab

    Return _ (Just expr) -> do
        rt <- gets curRetty
        assert (rt /= Void) "Cannot return expression in void function."
        collectExpr expr
        collect (typeOf expr) rt

    Extern _ _ _ _ _ -> return ()
    
    If _ cond blk melse -> do
        collectCondition cond
        collectStmt blk
        maybe (return ()) collectStmt melse

    Assign _ pattern expr -> collectExpr expr >> collectPattern pattern (typeOf expr)

    Set _ index expr -> do
        tm <- collectIndex index
        when (isJust tm) $ collect (fromJust tm) (typeOf expr)
        collectExpr expr

    AppendStmt app -> void (collectAppend app)

    For _ idxStr expr guardm blk -> do
        define idxStr KeyVar (ObjVar I64)
        collectExpr expr
        maybe (return ()) collectExpr guardm
        collectStmt blk

    Switch _ expr cases -> do
        collectExpr expr
        forM_ cases $ \(pattern, blk) -> do
            pushSymTab
            collectPattern pattern (typeOf expr)
            collectStmt blk
            popSymTab

    While _ cond blk -> do
        collectCondition cond
        collectStmt blk

    CallStmt p sym exprs -> do
        mapM_ collectExpr exprs
        -- TODO resm <- lookm (Sym sym) (KeyFunc $ map typeOf exprs)


    _ -> fail (show stmt)


collectAppend :: BoM CollectState m => Append -> m (Maybe Type)
collectAppend append = withPos append $ case append of
    AppendTable _ app expr -> do
        tm <- collectAppend app
        when (isJust tm) $ collect (fromJust tm) (typeOf expr)
        return tm

    AppendElem _ app expr -> do
        tm <- collectAppend app
        case tm of
            Just (T.Table [te]) -> collect te (fromJust tm) >> return tm
            _                   -> return tm

    AppendIndex index -> collectIndex index


collectIndex :: BoM CollectState m => Index -> m (Maybe Type)
collectIndex index = withPos index $ case index of
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
collectPattern pattern typ = withPos pattern $ case pattern of
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
collectExpr expr = withPos expr $ case expr of
    AExpr typ (Conv p t [e]) -> do
        collect typ t
        collectExpr e

    AExpr typ (Call p sym []) -> do
        ObjFunc rt <- look sym (KeyFunc [])
        collect typ rt

    AExpr typ (Ident p sym) -> do
        ObjVar t <- look sym KeyVar
        collect t typ

    _ -> return ()
    
    _ -> fail (show expr)
