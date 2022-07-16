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
import Interop

import qualified Debug.Trace

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
        , typeSupply :: Int
        }

initCollectState imp = CollectState
    { symTab     = SymTab.initSymTab
    , curRetty   = Void
    , collected  = []
    , defaults   = []
    , imports    = imp
    , curPos     = TextPos "" 0 0 0
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

    SymQualified mod sym -> do
        statem <- Map.lookup mod <$> gets imports
        case statem of
            Nothing -> fail $ mod ++ " not imported"
            Just state -> do
                return $ SymTab.lookupSym sym state
        

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


collectCExterns :: BoM CollectState m => [Extern] -> m ()
collectCExterns externs = do
    forM_ externs $ \extern -> case extern of
        ExtVar sym (AnnoType typ) -> define sym KeyVar (ObjVar typ)
        ExtFunc sym argTypes retty -> define sym (KeyFunc argTypes) (ObjFunc retty)

collectAST :: BoM CollectState m => AST -> m ()
collectAST ast = do
    let (typedefs, stmts'') = partition isTypedef (S.astStmts ast)
    let (funcdefs, stmts) = partition isFuncdef stmts''

    forM typedefs $ collectTypedef

    forM funcdefs $ \(S.FuncDef pos (Sym sym) params retty _) -> collectPos pos $
        define sym (KeyFunc $ map paramType params) (ObjFunc retty)

    mapM_ collectStmt stmts''
    where
        isTypedef :: Stmt -> Bool
        isTypedef (S.Typedef _ _ _) = True
        isTypedef _                 = False

        isFuncdef :: Stmt -> Bool
        isFuncdef (S.FuncDef _ _ _ _ _) = True
        isFuncdef _                     = False

collectTypedef :: BoM CollectState m => Stmt -> m ()
collectTypedef (S.Typedef pos (Sym sym) annoTyp) = collectPos pos $ case annoTyp of
    AnnoType t   ->
        define sym KeyType (ObjType t)

    AnnoTuple xs   -> do
        let ts = map snd xs
        let typedef = T.Typedef (Sym sym)
        forM_ (zip xs [0..]) $ \((s, t), i) -> define s (KeyMember typedef) (ObjMember i)
        define sym KeyType $ ObjType $ T.Tuple (map snd xs)
        define sym (KeyFunc ts) (ObjFunc typedef) 

    AnnoADT xs -> do
        let ts = map snd xs
        let typedef = T.Typedef (Sym sym)
        forM_ (zip xs [0..]) $ \(((Sym s), t), i) -> do
            define s (KeyMember typedef) (ObjMember i)
            define s (KeyFunc [t]) (ObjFunc typedef)
        define sym KeyType $ ObjType $ T.ADT (map snd xs)



collectStmt :: BoM CollectState m => Stmt -> m ()
collectStmt stmt = collectPos stmt $ case stmt of
    S.Typedef _ _ _ -> collectTypedef stmt
    Print p exprs -> mapM_ collectExpr exprs
    S.Typedef _ _ _ -> return ()
    Block stmts -> pushSymTab >> mapM_ collectStmt stmts >> popSymTab

    FuncDef _ sym params retty blk -> do
        oldRetty <- gets curRetty
        modify $ \s -> s { curRetty = retty }
        pushSymTab
        forM_ params $ \(Param _ (Sym s) t) ->
            define s KeyVar (ObjVar t)
        collectStmt blk
        popSymTab
        modify $ \s -> s { curRetty = oldRetty }
        collectDefault retty T.Void

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

    Switch p expr cases -> do
        collectExpr expr
        pushSymTab
        forM_ cases $ \(pat, stmt) -> do
            collectPattern pat (typeOf expr)
            collectStmt stmt
        popSymTab

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
    IndIdent _ symbol -> do
        ObjVar t <- look symbol KeyVar
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
    PatIdent _ (Sym s) -> do
        define s KeyVar (ObjVar typ)

    PatLiteral expr -> collect typ (typeOf expr)

    PatGuarded _ pat expr -> do
        collectPattern pat typ
        collectExpr expr

    PatField _ symbol pat -> do
        base <- baseTypeOf typ
        case base of
            ADT ts -> do
                ObjMember i <- look symbol (KeyMember typ)
                collectPattern pat (ts !! i)
            _ -> collectPattern pat =<< genType

    PatTuple _ pats -> do
        base <- baseTypeOf typ
        case base of
            T.Tuple ts -> do
                assert (length ts == length pats) "Invalid tuple pattern"
                zipWithM_ collectPattern pats ts
            _ -> do
                gts <- replicateM (length pats) genType
                zipWithM_ collectPattern pats gts
                collect base (T.Tuple gts)

    _ -> fail $ "cannot collect: " ++ show pattern


collectCondition :: BoM CollectState m => Condition -> m ()
collectCondition cond = case cond of
    CondExpr expr -> do
        collectDefault (typeOf expr) T.Bool
        collectExpr expr


collectCallExpr :: BoM CollectState m => Expr -> m ()
collectCallExpr (AExpr exprType (Call p symbol es)) = do
        kos <- lookSym symbol

        let rtm = rettySame (map snd kos)
        let odm = oneDef kos
        let rmm = rettyMatchesOne exprType kos
        let amm = argsMatch (map typeOf es) kos
        let alm = argLengthsMatch (map typeOf es) kos

        when (isJust rtm) $ do
            collect exprType (fromJust rtm)

        when (isJust odm) $ do
            let (ts, rt) = fromJust odm
            assert (length ts == length es) "Invalid arguments"
            zipWithM_  collect ts (map typeOf es)
            collect exprType rt

        when (isJust rmm) $ do
            let (ts, rt) = fromJust rmm
            assert (length ts == length es) "Invalid arguments"
            zipWithM_  collect ts (map typeOf es)

        when (isJust amm) $ do
            collect exprType (fromJust amm)

        when (isJust alm) $ do
            let (ts, rt) = fromJust alm
            zipWithM_  collect ts (map typeOf es)
            collect exprType rt

        mapM_ collectExpr es
        where
            rettySame :: [Object] -> Maybe Type
            rettySame []              = Nothing
            rettySame (ObjFunc rt:xs) = case rettySame xs of
                Just rt' | rt' == rt -> Just rt
                _                    -> Nothing
            rettySame (_:xs) = rettySame xs


            oneDef :: [(SymKey, Object)] -> Maybe ([Type], Type)
            oneDef [(KeyFunc ts, ObjFunc rt)] = Just (ts, rt)
            oneDef _                          = Nothing


            rettyMatchesOne :: Type -> [(SymKey, Object)] -> Maybe ([Type], Type)
            rettyMatchesOne _ [] = Nothing
            rettyMatchesOne typ ((KeyFunc ts, ObjFunc rt):xs)
                | typ == rt = case rettyMatchesOne typ xs of
                    Nothing -> Just (ts, rt)
                    Just _  -> Nothing
                | otherwise = rettyMatchesOne typ xs
            rettyMatchesOne typ (_:xs) = rettyMatchesOne typ xs


            argsMatch :: [Type] -> [(SymKey, Object)] -> Maybe Type
            argsMatch _ [] = Nothing
            argsMatch ats ((KeyFunc ts, ObjFunc rt):xs)
                | ats == ts = case argsMatch ats xs of
                    Nothing -> Just rt
                    Just _  -> Nothing
                | otherwise = argsMatch ats xs
            argsMatch ats (_:xs) = argsMatch ats xs


            argLengthsMatch :: [Type] -> [(SymKey, Object)] -> Maybe ([Type], Type)
            argLengthsMatch ats [] = Nothing
            argLengthsMatch ats ((KeyFunc ts, ObjFunc rt):xs)
                | length ats == length ts = case argLengthsMatch ats xs of
                    Nothing -> Just (ts, rt)
                    _       -> Nothing
                | otherwise        = argLengthsMatch ats xs
            argLengthsMatch ats (_:xs) = argLengthsMatch ats xs


collectExpr :: BoM CollectState m => Expr -> m ()
collectExpr (AExpr exprType expr) = collectPos expr $ case expr of
    Call p symbol es -> collectCallExpr (AExpr exprType expr)
    Conv p t [e] -> collect exprType t >> collectExpr e
    S.Char p c -> collectDefault exprType T.Char
    S.Int p c -> collectDefault exprType I64
    S.Prefix p op e -> collect exprType (typeOf e) >> collectExpr e
    S.Copy p e -> collect exprType (typeOf e) >> collectExpr e
    S.Len p e -> collectDefault exprType I64 >> collectExpr e
    S.Bool p b -> collectDefault exprType T.Bool
    S.String p s -> collectDefault exprType (T.Table [T.Char])
    S.Float p f -> collectDefault exprType F64

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

    S.Subscript p e1 e2 -> do
        case typeOf e1 of
            Type x      -> return ()
            T.Table [t] -> collect exprType t
            _           -> error $ show (typeOf e1)
        collectExpr e1
        collectExpr e2


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


    S.AExpr _ _ -> fail "what"

    _ -> fail ("collect: " ++ show expr)
