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

import qualified Debug.Trace

-- constraints obtained from sub-expressions must be to the left
data Constraint
    = ConsEq Type Type
    | ConsBase   Type Type -- both types must have same base
    | ConsElem   Type Type -- both types must have same base
    | ConsMember Type Int Type 
    | ConsAdtMem Type Int Int Type
    deriving (Show, Eq, Ord)

type SymTab = SymTab.SymTab Symbol SymKey Object

data SymKey
    = KeyVar
    | KeyType
    | KeyFunc [Type] Type
    | KeyMember Type
    | KeyAdtField
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

collectAdtMember :: BoM CollectState m => Type -> Int -> Int -> Type -> m () 
collectAdtMember t i j agg =
    modify $ \s -> s { collected = (curPos s, ConsAdtMem t i j agg) : (collected s) }

collectMember :: BoM CollectState m => Type -> Int -> Type -> m () 
collectMember t i agg =
    modify $ \s -> s { collected = (curPos s, ConsMember t i agg) : (collected s) }


collect :: BoM CollectState m => Constraint -> m ()
collect constraint = do
    modify $ \s -> s { collected = (curPos s, constraint) : (collected s) }

collectBase :: BoM CollectState m => Type -> Type -> m ()
collectBase t1 t2 = do
    modify $ \s -> s { collected = (curPos s, ConsBase t1 t2) : (collected s) }

collectElem :: BoM CollectState m => Type -> Type -> m ()
collectElem t1 t2 = do
    modify $ \s -> s { collected = (curPos s, ConsElem t1 t2) : (collected s) }

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
        ExtFunc sym argTypes retty  -> define (SymQualified "c" sym) (KeyFunc argTypes retty) ObjFunc
        ExtConstInt sym n           -> define (SymQualified "c" sym) KeyVar (ObjVar I64)
        ExtTypeDef sym typ          -> define (SymQualified "c" sym) KeyType (ObjType typ)

collectAST :: BoM CollectState m => S.AST -> m ()
collectAST ast = do
    let (typedefs, stmts'') = partition isTypedef (S.astStmts ast)
    let (funcdefs, stmts) = partition isFuncdef stmts''

    forM typedefs $ collectTypedef

    forM funcdefs $ \(S.FuncDef pos sym params retty _) -> collectPos pos $
        define (Sym sym) (KeyFunc (map S.paramType params) retty) ObjFunc

    mapM_ collectStmt stmts''
    where
        isTypedef :: S.Stmt -> Bool
        isTypedef (S.Typedef _ _ _) = True
        isTypedef _                 = False

        isFuncdef :: S.Stmt -> Bool
        isFuncdef (S.FuncDef _ _ _ _ _) = True
        isFuncdef _                     = False

collectTypedef :: BoM CollectState m => S.Stmt -> m ()
collectTypedef (S.Typedef pos symbol annoTyp) = collectPos pos $ case annoTyp of
    S.AnnoType (Tuple ts) -> do
        let typedef = Typedef symbol
        define symbol KeyType (ObjType $ Tuple ts)
        define symbol (KeyFunc ts typedef) ObjFunc
        
    S.AnnoType t   -> do
        let typedef = Typedef symbol
        define symbol KeyType (ObjType t)
        define symbol (KeyFunc [t] typedef) ObjFunc

    S.AnnoTuple xs   -> do
        let typedef = Typedef symbol
        let ts = map snd xs
        forM_ (zip xs [0..]) $ \((s, t), i) -> define (Sym s) (KeyMember typedef) (ObjMember i)
        define symbol KeyType $ ObjType $ Tuple (map snd xs)
        define symbol (KeyFunc ts typedef) ObjFunc

    S.AnnoADT xs -> do
        let typedef = Typedef symbol
        fs <- forM (zip xs [0..]) $ \(x, i) -> case x of
            S.ADTFieldMember s ts -> do
                define s KeyAdtField (ObjMember i)
                define s (KeyFunc ts typedef) ObjFunc
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

    S.FuncDef _ sym params retty blk -> do
        oldRetty <- gets curRetty
        modify $ \s -> s { curRetty = retty }
        forM_ params $ \(S.Param _ symbol t) ->
            define symbol KeyVar (ObjVar t)
        collectStmt blk
        modify $ \s -> s { curRetty = oldRetty }
        collectDefault retty Void

    S.Return _ mexpr -> do
        retty <- gets curRetty
        case mexpr of
            Nothing -> collect $ ConsEq Void retty
            Just expr -> do
                collect $ ConsEq (typeOf expr) retty
                collectExpr expr

    S.If _ cond blk melse -> do
        collectCondition cond
        collectStmt blk
        maybe (return ()) collectStmt melse

    S.Assign _ pattern expr -> do
        collectPattern pattern (typeOf expr)
        collectExpr expr

    S.Set _ index expr -> do
        typ <- collectIndex index
        collect $ ConsEq typ (typeOf expr)
        collectExpr expr

    S.AppendStmt app -> void (collectAppend app)

    S.While _ cond blk -> do
        collectCondition cond
        collectStmt blk

    S.CallStmt p symbol es -> do
        kos <- lookSym symbol
        case kos of
            -- no definitions 
            [] -> fail $ show symbol ++ " undefined"
            -- one definition
            [(KeyFunc ts rt, ObjFunc)] -> do
                assert (length ts == length es) "Invalid arguments"
                mapM_ collect $ zipWith ConsEq ts (map typeOf es)

            -- do nothing
            _ -> return ()

        mapM_ collectExpr es

    S.Switch p expr cases -> do
        collectExpr expr
        forM_ cases $ \(pat, stmt) -> do
            collectPattern pat (typeOf expr)
            collectStmt stmt

    S.For p symbol (Just t) expr mpat blk -> do
        define symbol KeyVar (ObjVar t)
        collectDefault t I64
        collectExpr expr

        when (isJust mpat) $ do
            gt <- genType
            collectElem gt (typeOf expr)
            collectPattern (fromJust mpat) gt

        collectStmt blk

    S.Data p symbol typ -> do
        define symbol KeyVar (ObjVar typ)
        

    _ -> error (show stmt)


-- return type of append result
collectAppend :: BoM CollectState m => S.Append -> m Type
collectAppend append = collectPos append $ case append of
    S.AppendTable _ app expr -> do
        t <- collectAppend app
        collect $ ConsEq t (typeOf expr)
        collectExpr expr
        return t

    S.AppendIndex index -> collectIndex index


-- returns type of resulting index
collectIndex :: BoM CollectState m => S.Index -> m Type
collectIndex index = collectPos index $ case index of
    S.IndIdent _ symbol -> do
        ObjVar t <- look symbol KeyVar
        return t

    S.IndArray _ ind expr -> do
        gt <- genType
        collectElem gt =<< collectIndex ind
        collectExpr expr
        return gt

    _ -> error (show index)


-- collectPattern pattern <with this type of expression trying to match>
collectPattern :: BoM CollectState m => S.Pattern -> Type -> m ()
collectPattern pattern typ = collectPos pattern $ case pattern of
    S.PatIgnore pos -> return ()
    S.PatIdent _ symbol -> do
        define symbol KeyVar (ObjVar typ)

    S.PatLiteral expr -> collect $ ConsEq typ (typeOf expr)

    S.PatGuarded _ pat expr -> do
        collectPattern pat typ
        collectBase (typeOf expr) Bool
        collectExpr expr

    S.PatField _ symbol pats -> do
        resm <- lookm symbol KeyAdtField
        case resm of
            Just (ObjMember i) -> do
                gts <- replicateM (length pats) genType
                zipWithM_ collectPattern pats gts
                forM_ (zip gts [0..]) $ \(t, j) -> collectAdtMember t i j typ
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
        collect $ ConsEq t typ
        collectPattern pat typ

    S.PatNull _ -> return ()
        

    _ -> error $ show pattern


collectCondition :: BoM CollectState m => S.Condition -> m ()
collectCondition cond = case cond of
    S.CondExpr expr -> do
        collectDefault (typeOf expr) Bool
        collectBase (typeOf expr) Bool
        collectExpr expr

    S.CondMatch pat expr -> do
        collectPattern pat (typeOf expr)
        collectExpr expr
        


collectCallExpr :: BoM CollectState m => S.Expr -> m ()
collectCallExpr (S.AExpr exprType (S.Call p symbol es)) = do
        kos <- lookSym symbol

        let rtm = rettySame kos
        let odm = oneDef kos
        let rmm = rettyMatchesOne exprType kos
        let amm = argsMatch (map typeOf es) kos
        let alm = argLengthsMatch (map typeOf es) kos

        when (isJust rtm) $ do
            collect $ ConsEq exprType (fromJust rtm)

        when (isJust odm) $ do
            let (ts, rt) = fromJust odm
            assert (length ts == length es) "Invalid arguments"
            mapM_  collect $ zipWith ConsEq ts (map typeOf es)
            collect $ ConsEq exprType rt

        when (isJust rmm) $ do
            let (ts, rt) = fromJust rmm
            assert (length ts == length es) "Invalid arguments"
            mapM_ collect $ zipWith ConsEq ts (map typeOf es)

        when (isJust amm) $ do
            collect $ ConsEq exprType (fromJust amm)

        when (isJust alm) $ do
            let (ts, rt) = fromJust alm
            mapM_ collect $ zipWith ConsEq ts (map typeOf es)
            collect $ ConsEq exprType rt

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


collectExpr :: BoM CollectState m => S.Expr -> m ()
collectExpr (S.AExpr exprType expr) = collectPos expr $ case expr of
    S.Call p symbol es -> collectCallExpr (S.AExpr exprType expr)
    S.Conv p t [e]     -> collect (ConsEq exprType t) >> collectExpr e
    S.Int p c        -> collectDefault exprType I64
    S.Prefix p op e  -> collect (ConsEq exprType (typeOf e)) >> collectExpr e
    S.Copy p e       -> collect (ConsEq exprType (typeOf e)) >> collectExpr e
    S.Len p e        -> collectDefault exprType I64 >> collectExpr e
    S.Float p f      -> collectDefault exprType F64
    S.Zero p         -> collectDefault exprType (Tuple [])

    S.Char p c       -> do
        collectBase exprType Char
        collectDefault exprType Char

    S.Bool p b       -> do
        collectBase exprType Bool
        collectDefault exprType Bool

    S.String p s     -> do
        collectBase exprType (Table [Char])
        collectDefault exprType (Table [Char])

    S.UnsafePtr p e -> do
        collect $ ConsEq exprType (UnsafePtr (typeOf e))
        collectExpr e

    S.Ident p symbol -> do
        ObjVar t <- look symbol KeyVar
        collect $ ConsEq t exprType

    S.Infix p op e1 e2 -> do
        case op of
            _ | op `elem` [S.Plus, S.Minus, S.Times, S.Divide, S.Modulo] -> do
                collect $ ConsEq exprType (typeOf e1)
            _ | op `elem` [S.LT, S.GT, S.LTEq, S.GTEq, S.EqEq, S.NotEq]  -> do
                collectBase exprType Bool
                collectDefault exprType Bool
            _ | op `elem` [S.AndAnd, S.OrOr] -> do
                collectBase exprType Bool
                collect $ ConsEq exprType (typeOf e1)
            _ -> return ()
                    
        collect $ ConsEq (typeOf e1) (typeOf e2)
        collectExpr e1
        collectExpr e2

    S.Subscript p e1 e2 -> do
        collectElem exprType (typeOf e1)
        collectExpr e1
        collectExpr e2

    S.Table p [[]] -> collectDefault exprType (Table [Tuple []])

    S.Table p [es] -> do
        mapM_ (\e -> collectElem e exprType) (map typeOf es)
        case es of
            (x:xs) -> do
                collectDefault exprType $ Table [typeOf x]
                collectBase exprType $ Table [typeOf x]
            _ -> return ()
            
        mapM_ collectExpr es


    S.Tuple p es -> do
        collectBase exprType $ Tuple (map typeOf es)
        collectDefault exprType $ Tuple (map typeOf es)
        mapM_ collectExpr es

    S.Member p e sym -> do
        case typeOf e of
            Type x         -> return ()
            Typedef symbol -> do
                ObjMember i  <- look (Sym sym) $ KeyMember (typeOf e)
                collectMember exprType i (typeOf e)

        collectExpr e

    S.TupleIndex p e i -> do
        collectMember exprType (fromIntegral i) (typeOf e)
        collectExpr e

    S.Range p e me1 me2 -> do
        collect $ ConsEq exprType (typeOf e)
        collectExpr e

        when (isJust me1) $ do
            collectDefault (typeOf $ fromJust me1) I64
            collectExpr (fromJust me1)

        when (isJust me2) $ do
            collectDefault (typeOf $ fromJust me2) I64
            collectExpr (fromJust me2)

        when (isJust me1 && isJust me2) $
            collect $ ConsEq (typeOf $ fromJust me1) (typeOf $ fromJust me2)

    S.Null p -> return ()


    S.AExpr _ _ -> fail "what"

    S.ADT p e -> do
        collectExpr e

    _ -> error (show expr)
