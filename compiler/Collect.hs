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
import Symbol

import qualified Debug.Trace

-- constraints obtained from sub-expressions must be to the left
data Constraint
    = Constraint TextPos Type Type
    deriving (Show, Eq)

instance TextPosition Constraint where
    textPos (Constraint pos _ _) = pos

type SymTab = SymTab.SymTab Symbol SymKey Object

data SymKey
    = KeyVar
    | KeyType
    | KeyFunc [Type] Type
    | KeyMember Type
    deriving (Show, Eq, Ord)

data Object
    = ObjVar Type
    | ObjType Type
    | ObjFunc 
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
        , modName   :: String
        }

initCollectState imp mod = CollectState
    { symTab     = SymTab.initSymTab
    , curRetty   = Void
    , collected  = []
    , defaults   = []
    , imports    = imp
    , curPos     = TextPos "" 0 0 0
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
lookm symbol key = do
    kos <- lookSym symbol
    case map snd $ filter ((== key) . fst) kos of
        [] -> return Nothing
        [o] -> return (Just o)
        _ -> fail $ show symbol ++ " is ambiguous"


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


baseTypeOf :: BoM CollectState m => Type -> m Type
baseTypeOf typ = case typ of
    T.Typedef sym -> do ObjType t <- look sym KeyType; baseTypeOf t
    Type x        -> return (Type undefined)
    _             -> return typ


collectCExterns :: BoM CollectState m => [Extern] -> m ()
collectCExterns externs = do
    forM_ externs $ \extern -> case extern of
        ExtVar sym (AnnoType typ)  -> define (SymQualified "c" sym) KeyVar (ObjVar typ)
        ExtFunc sym argTypes retty -> define (SymQualified "c" sym) (KeyFunc argTypes retty) ObjFunc
        ExtConstInt sym n          -> define (SymQualified "c" sym) KeyVar (ObjVar I64)
        ExtTypeDef sym typ         -> define (SymQualified "c" sym) KeyType (ObjType typ)

collectAST :: BoM CollectState m => AST -> m ()
collectAST ast = do
    let (typedefs, stmts'') = partition isTypedef (S.astStmts ast)
    let (funcdefs, stmts) = partition isFuncdef stmts''

    forM typedefs $ collectTypedef

    forM funcdefs $ \(S.FuncDef pos sym params retty _) -> collectPos pos $
        define (Sym sym) (KeyFunc (map paramType params) retty) ObjFunc

    mapM_ collectStmt stmts''
    where
        isTypedef :: Stmt -> Bool
        isTypedef (S.Typedef _ _ _) = True
        isTypedef _                 = False

        isFuncdef :: Stmt -> Bool
        isFuncdef (S.FuncDef _ _ _ _ _) = True
        isFuncdef _                     = False

collectTypedef :: BoM CollectState m => Stmt -> m ()
collectTypedef (S.Typedef pos symbol annoTyp) = collectPos pos $ case annoTyp of
    AnnoType t   ->
        define symbol KeyType (ObjType t)

    AnnoTuple xs   -> do
        let typedef = T.Typedef symbol
        let ts = map snd xs
        forM_ (zip xs [0..]) $ \((s, t), i) -> define (Sym s) (KeyMember typedef) (ObjMember i)
        define symbol KeyType $ ObjType $ T.Tuple (map snd xs)
        define symbol (KeyFunc ts typedef) ObjFunc

    AnnoADT xs -> do
        let typedef = T.Typedef symbol
        forM_ (zip xs [0..]) $ \((s, ts), i) -> do
            define s (KeyMember typedef) (ObjMember i)
            define s (KeyFunc ts typedef) ObjFunc
        define symbol KeyType $ ObjType $ T.ADT (map snd xs)



collectStmt :: BoM CollectState m => Stmt -> m ()
collectStmt stmt = collectPos stmt $ case stmt of
    S.Typedef _ _ _ -> collectTypedef stmt
    Print p exprs -> mapM_ collectExpr exprs
    S.Typedef _ _ _ -> return ()
    Block stmts -> mapM_ collectStmt stmts

    FuncDef _ sym params retty blk -> do
        oldRetty <- gets curRetty
        modify $ \s -> s { curRetty = retty }
        forM_ params $ \(Param _ symbol t) ->
            define symbol KeyVar (ObjVar t)
        collectStmt blk
        modify $ \s -> s { curRetty = oldRetty }
        collectDefault retty T.Void

    Return _ mexpr -> do
        case mexpr of
            Nothing -> collect Void =<< gets curRetty
            Just expr -> do
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

    CallStmt p symbol es -> do
        kos <- lookSym symbol
        case kos of
            -- no definitions 
            [] -> fail $ show symbol ++ " undefined"
            -- one definition
            [(KeyFunc ts rt, ObjFunc)] -> do
                assert (length ts == length es) "Invalid arguments"
                zipWithM_  collect ts (map typeOf es)

            -- do nothing
            _ -> return ()

        mapM_ collectExpr es

    Switch p expr cases -> do
        collectExpr expr
        forM_ cases $ \(pat, stmt) -> do
            collectPattern pat (typeOf expr)
            collectStmt stmt

    For p symbol (Just t) expr mpat blk -> do
        define symbol KeyVar (ObjVar t)
        collectDefault t I64
        collectExpr expr

        when (isJust mpat) $ do
            base <- baseTypeOf (typeOf expr)
            case base of
                T.Type x    -> collectPattern (fromJust mpat) =<< genType
                T.Table [t] -> collectPattern (fromJust mpat) t
                T.Array n t -> collectPattern (fromJust mpat) t

        collectStmt blk
        

    _ -> error (show stmt)


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

    _ -> error (show index)


-- collectPattern pattern <with this type of expression trying to match>
collectPattern :: BoM CollectState m => Pattern -> Type -> m ()
collectPattern pattern typ = collectPos pattern $ case pattern of
    PatIgnore pos -> return ()
    PatIdent _ symbol -> do
        define symbol KeyVar (ObjVar typ)

    PatLiteral expr -> collect typ (typeOf expr)

    PatGuarded _ pat expr -> do
        collectPattern pat typ
        collectExpr expr

    PatField _ symbol pats -> do
        base <- baseTypeOf typ
        case base of
            ADT tss -> do
                ObjMember i <- look symbol (KeyMember typ)
                let ts = tss !! i
                assert (length ts == length pats) "Invalid field"
                zipWithM_ collectPattern pats ts
            _ -> forM_ pats $ \pat -> collectPattern pat =<< genType

    PatTuple _ pats -> do
        base <- baseTypeOf typ
        case base of
            T.Tuple ts -> do
                assert (length ts == length pats) "Invalid tuple pattern"
                zipWithM_ collectPattern pats ts
            _ -> do
                gts <- replicateM (length pats) genType
                collectDefault typ (T.Tuple gts)
                zipWithM_ collectPattern pats gts

    PatArray _ pats -> do
        base <- baseTypeOf typ
        case base of
            T.Array n t -> do
                assert (n == length pats) "Invalid array pattern"
                mapM_ (\p -> collectPattern p t) pats
            _ -> do
                t <- genType
                mapM_ (\p -> collectPattern p t) pats

    PatAnnotated pat t -> do
        collect t typ
        collectPattern pat typ
        

    _ -> error $ show pattern


collectCondition :: BoM CollectState m => Condition -> m ()
collectCondition cond = case cond of
    CondExpr expr -> do
        collectDefault (typeOf expr) T.Bool
        collectExpr expr

    CondMatch pat expr -> do
        collectPattern pat (typeOf expr)
        collectExpr expr
        


collectCallExpr :: BoM CollectState m => Expr -> m ()
collectCallExpr (AExpr exprType (Call p symbol es)) = do
        kos <- lookSym symbol

        let rtm = rettySame kos
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
            rettySame :: [(SymKey, Object)] -> Maybe Type
            rettySame []                            = Nothing
            rettySame ((KeyFunc ts rt, ObjFunc):xs) = case rettySame xs of
                Just rt' | rt' == rt -> Just rt
                _                    -> Nothing
            rettySame (_:xs) = rettySame xs


            oneDef :: [(SymKey, Object)] -> Maybe ([Type], Type)
            oneDef [(KeyFunc ts rt, ObjFunc)] = Just (ts, rt)
            oneDef _                          = Nothing


            rettyMatchesOne :: Type -> [(SymKey, Object)] -> Maybe ([Type], Type)
            rettyMatchesOne _ [] = Nothing
            rettyMatchesOne typ ((KeyFunc ts rt, ObjFunc):xs)
                | typ == rt = case rettyMatchesOne typ xs of
                    Nothing -> Just (ts, rt)
                    Just _  -> Nothing
                | otherwise = rettyMatchesOne typ xs
            rettyMatchesOne typ (_:xs) = rettyMatchesOne typ xs


            argsMatch :: [Type] -> [(SymKey, Object)] -> Maybe Type
            argsMatch _ [] = Nothing
            argsMatch ats ((KeyFunc ts rt, ObjFunc):xs)
                | ats == ts = case argsMatch ats xs of
                    Nothing -> Just rt
                    Just _  -> Nothing
                | otherwise = argsMatch ats xs
            argsMatch ats (_:xs) = argsMatch ats xs


            argLengthsMatch :: [Type] -> [(SymKey, Object)] -> Maybe ([Type], Type)
            argLengthsMatch ats [] = Nothing
            argLengthsMatch ats ((KeyFunc ts rt, ObjFunc):xs)
                | length ats == length ts = case argLengthsMatch ats xs of
                    Nothing -> Just (ts, rt)
                    _       -> Nothing
                | otherwise        = argLengthsMatch ats xs
            argLengthsMatch ats (_:xs) = argLengthsMatch ats xs


collectExpr :: BoM CollectState m => Expr -> m ()
collectExpr (AExpr exprType expr) = collectPos expr $ case expr of
    Call p symbol es -> collectCallExpr (AExpr exprType expr)
    Conv p t [e]     -> collect exprType t >> collectExpr e
    S.Char p c       -> collectDefault exprType T.Char
    S.Int p c        -> collectDefault exprType I64
    S.Prefix p op e  -> collect exprType (typeOf e) >> collectExpr e
    S.Copy p e       -> collect exprType (typeOf e) >> collectExpr e
    S.Len p e        -> collectDefault exprType I64 >> collectExpr e
    S.Bool p b       -> collectDefault exprType T.Bool
    S.String p s     -> collectDefault exprType (T.Table [T.Char])
    S.Float p f      -> collectDefault exprType F64
    S.Zero p         -> collectDefault exprType (T.Tuple [])

    S.UnsafePtr p e -> do
        collect exprType (T.UnsafePtr (typeOf e))
        collectExpr e

    Ident p symbol -> do
        ObjVar t <- look symbol KeyVar
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
        base <- baseTypeOf (typeOf e1)
        case base of
            Type undef  -> return ()
            T.Table [t] -> collect exprType t
            Array n t   -> collect exprType t
            _           -> fail $ "invalid type: " ++ show base

        collectExpr e1
        collectExpr e2

    --S.Table p [[]] -> collectDefault exprType (T.Table [T.Tuple []])

    S.Table p [es] -> do
        base <- baseTypeOf exprType
        case base of
            T.Table [t] -> mapM_ (collect t) (map typeOf es)
            Array n t   -> mapM_ (collect t) (map typeOf es)
            _           -> return ()

        case es of
            -- TODO current default system causes this to break inference.
            -- Suggest to choose defaults after cycling the type inference.
            [] -> return () --collectDefault exprType $ T.Table [T.Tuple []] 
            (x:xs) -> do
                mapM (collect (typeOf x)) (map typeOf xs)
                collectDefault exprType $ T.Table [typeOf x]

        mapM_ collectExpr es


    S.Tuple p es -> do
        base <- baseTypeOf exprType
        case base of
            T.Tuple ts -> zipWithM_ collect ts (map typeOf es)
            _          -> return ()
        collectDefault exprType $ T.Tuple (map typeOf es)
        mapM_ collectExpr es

    Member p e sym -> do
        case typeOf e of
            Type x           -> return ()
            T.Typedef symbol -> do
                ObjMember i  <- look (Sym sym) $ KeyMember (typeOf e)
                ObjType base <- look symbol KeyType
                case base of
                    T.Tuple ts -> collect exprType (ts !! i)

        collectExpr e

    TupleIndex p e i -> do
        collectExpr e
        base <- baseTypeOf (typeOf e)
        case base of
            T.Tuple ts -> collect exprType (ts !! fromIntegral i)
            _          -> return ()

    Range p e me1 me2 -> do
        base <- baseTypeOf (typeOf e)
        case base of
            T.Table ts -> collect exprType (typeOf e)
            Array n t  -> collect exprType (typeOf e)
            Type undef -> collect exprType (typeOf e)
            _          -> fail $ "cannot take range of expression"

        collectExpr e

        when (isJust me1) $ do
            collectDefault (typeOf $ fromJust me1) I64
            collectExpr (fromJust me1)

        when (isJust me2) $ do
            collectDefault (typeOf $ fromJust me2) I64
            collectExpr (fromJust me2)

        when (isJust me1 && isJust me2) $
            collect (typeOf $ fromJust me1) (typeOf $ fromJust me2)


    S.AExpr _ _ -> fail "what"

    _ -> error (show expr)
