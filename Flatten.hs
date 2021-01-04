{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Flatten where
-- Walks an AST and resolves all symbols into unique names depending on scope.

import Control.Monad.State 
import Control.Monad.Fail hiding (fail)
import Data.Maybe
import qualified Data.Set as Set 
import qualified Data.Map as Map 
import qualified AST as S
import qualified Type as T
import qualified SymTab
import Monad
import Error

type FlatSym = String


data SymKey
    = KeyType
    | KeyVar
    | KeyFunc
    | KeyExtern
    deriving (Show, Eq, Ord)


data SymObj
    = ObjTypeDef    TextPos T.Type
    | ObjVarDef  TextPos S.Expr
    | ObjFuncDef S.Stmt
    | ObjExtern  S.Stmt
    deriving (Show)


data FlattenState
    = FlattenState
        { imports    :: Set.Set S.ModuleName
        , defTab     :: Map.Map FlatSym SymObj
        , flatTab    :: Map.Map SymKey [FlatSym]
        , symTab     :: SymTab.SymTab S.Symbol (Map.Map SymKey FlatSym)
        , symSupply  :: Map.Map S.Symbol Int
        }


initFlattenState importFlatMap
    = FlattenState
        { imports    = Set.empty
        , defTab     = Map.empty
        , flatTab    = Map.empty
        , symTab     = SymTab.initSymTab
        , symSupply  = Map.empty
        }


flattenAST
    :: (MonadIO m, MonadFail m)
    => Map.Map S.ModuleName FlattenState
    -> S.AST
    -> m (Either CmpError FlattenState)
flattenAST importFlatMap ast = do
    res <- runBoMT (initFlattenState importFlatMap) $ do
        mapM_ gatherTopStmt (S.astStmts ast)
        mapM_ resolveTypedef =<< getFlats KeyType
        mapM_ checkTypedefCircles =<< getFlats KeyType
        mapM_ resolveVariable =<< getFlats KeyVar
        mapM_ resolveExtern =<< getFlats KeyExtern
        mapM_ resolveFunction =<< getFlats KeyFunc
    case res of
        Left err         -> return (Left err)
        Right (_, state) -> return (Right state { imports = Map.keysSet importFlatMap })
    where
        moduleName = maybe "main" id (S.astModuleName ast)
        
        gatherTopStmt :: BoM FlattenState m => S.Stmt -> m ()
        gatherTopStmt stmt = case stmt of
            S.Typedef pos sym typ -> do
                checkSymKeyUndef sym KeyType
                flat <- fresh (moduleName ++ "_type_" ++ sym)
                addSym sym KeyType flat
                addFlat KeyType flat
                addObj flat (ObjTypeDef pos typ)
            S.Func pos sym params retty blk -> do
                checkSymKeyUndef sym KeyFunc
                flat <- fresh (moduleName ++ "_fn_" ++ sym)
                addSym sym KeyFunc flat
                addFlat KeyFunc flat
                addObj flat (ObjFuncDef stmt) 
            S.Extern pos sym params retty -> do
                checkSymKeyUndef sym KeyFunc 
                addSym sym KeyFunc sym
                addFlat KeyExtern sym
                addObj sym (ObjExtern stmt) 
            S.Assign pos (S.PatIgnore _) expr ->
                return ()
            S.Assign pos (S.PatIdent p sym) expr -> do
                checkSymKeyUndef sym KeyVar
                flat <- fresh (moduleName ++ "_var_" ++ sym)
                addSym sym KeyVar flat
                addFlat KeyVar flat
                expr' <- resolveConstExpr expr
                addObj flat (ObjVarDef pos expr') 
            _ -> fail "invalid top-level statement"

        resolveTypedef :: BoM FlattenState m => FlatSym -> m ()
        resolveTypedef flat = do
            ObjTypeDef pos typ <- getObj flat
            typ' <- case typ of
                T.I32       -> return typ
                T.I64       -> return typ
                T.Bool      -> return typ
                T.Typedef s -> fmap T.Typedef (look s KeyType)
            addObj flat (ObjTypeDef pos typ')


        checkTypedefCircles :: BoM FlattenState m => FlatSym -> m ()
        checkTypedefCircles flat = do
            checkTypedefCircles' flat Set.empty
            where
                checkTypedefCircles' :: BoM FlattenState m => FlatSym -> Set.Set FlatSym -> m ()
                checkTypedefCircles' flat visited = do
                    when (Set.member flat visited) $
                        fail ("circular type dependency: " ++ flat)
                    ObjTypeDef pos typ <- getObj flat
                    case typ of
                        T.Typedef f -> checkTypedefCircles' f (Set.insert flat visited)
                        _           -> return ()


        fresh :: BoM FlattenState m => S.Symbol -> m FlatSym
        fresh sym = do
            res <- fmap (Map.lookup sym) (gets symSupply)
            let i = maybe 0 (+1) res
            modify $ \s -> s { symSupply = Map.insert sym i (symSupply s) }
            return (sym ++ "_" ++ show i)


        checkSymUndef :: BoM FlattenState m => S.Symbol -> m ()
        checkSymUndef sym = do
            res <- fmap (SymTab.lookupHead sym) (gets symTab)
            when (isJust res) $ fail (sym ++ " already defined")


        checkSymKeyUndef :: BoM FlattenState m => S.Symbol -> SymKey -> m ()
        checkSymKeyUndef sym key = do
            res <- fmap (SymTab.lookupHead sym) (gets symTab)
            case res of
                Nothing   -> return ()
                Just kmap -> when (isJust $ Map.lookup key kmap) $
                    fail (sym ++ " already defined for " ++ show key)


        lookImport :: BoM FlattenState m => S.Symbol -> SymKey -> m FlatSym
        lookImport sym key = do
            let symTabs = map symTab (Map.elems importFlatMap)
            let kmaps   = catMaybes $ map (SymTab.lookupHead sym) symTabs
            let results = catMaybes $ map (Map.lookup key) kmaps
            case results of
                [x] -> return x
                []  -> fail (sym ++ " isn't defined")
                n   -> fail (sym ++ " defined in multiple imported modules")
                

        look :: BoM FlattenState m => S.Symbol -> SymKey -> m FlatSym
        look sym key = do
            res <- fmap (SymTab.lookup sym) (gets symTab)
            case res of
                Nothing   -> lookImport sym key
                Just kmap -> do
                    let res = Map.lookup key kmap
                    when (isNothing res) $ fail (sym ++ " for " ++ show key ++ " isn't defined")
                    return (fromJust res)


        addSym :: BoM FlattenState m => S.Symbol -> SymKey -> FlatSym -> m ()
        addSym sym key flat = do
            res <- fmap (SymTab.lookup sym) (gets symTab)
            let kmap' = maybe Map.empty id res
            modify $ \s -> s { symTab  = SymTab.insert sym (Map.insert key flat kmap') (symTab s) }



        addFlat :: BoM FlattenState m => SymKey -> FlatSym -> m ()
        addFlat key flat = do
            mflats <- fmap (Map.lookup key) (gets flatTab)
            let flats' = maybe [flat] (flat:) mflats
            modify $ \s -> s { flatTab = Map.insert key flats' (flatTab s) }


        addObj :: BoM FlattenState m => FlatSym -> SymObj -> m ()
        addObj flat obj = do
            modify $ \s -> s { defTab  = Map.insert flat obj (defTab s) }



        getObj :: BoM FlattenState m => FlatSym -> m SymObj
        getObj flat = do
            res <- fmap (Map.lookup flat) (gets defTab)
            case res of
                Just obj -> return obj
                Nothing  -> do
                    let defTabs = map defTab $ (Map.elems importFlatMap)
                    return $ (Map.! flat) (foldr1 Map.union defTabs)



        getFlats :: BoM FlattenState m => SymKey -> m [FlatSym]
        getFlats key = do
            mflats <- fmap (Map.lookup key) (gets flatTab)
            return (maybe [] id mflats)


        pushScope :: BoM FlattenState m => m ()
        pushScope =
            modify $ \s -> s { symTab = SymTab.push (symTab s) }


        popScope :: BoM FlattenState m => m ()
        popScope =
            modify $ \s -> s { symTab = SymTab.pop (symTab s) }



        resolveVariable :: BoM FlattenState m => FlatSym -> m ()
        resolveVariable flat = do
            ObjVarDef pos expr <- getObj flat
            expr' <- resolveConstExpr expr
            addObj flat (ObjVarDef pos expr')


        resolveConstExpr :: BoM FlattenState m => S.Expr -> m S.Expr
        resolveConstExpr expr = case expr of
            S.Cons _          -> return expr
            S.Tuple pos exprs -> fmap (S.Tuple pos) (mapM resolveConstExpr exprs)


        resolvePattern :: BoM FlattenState m => S.Pattern -> m S.Pattern
        resolvePattern pat = case pat of
            S.PatIgnore pos    -> return pat
            S.PatIdent pos sym -> do
                checkSymKeyUndef sym KeyVar
                flat <- fresh sym
                addSym sym KeyVar flat
                return (S.PatIdent pos flat)
            S.PatLiteral cons ->
                return pat
            _ -> fail ("resolvePattern: " ++ show pat)


        resolveIndex :: BoM FlattenState m => S.Index -> m S.Index
        resolveIndex ind = case ind of
            S.IndIdent pos sym -> fmap (S.IndIdent pos) (look sym KeyVar)


        resolveExtern :: BoM FlattenState m => FlatSym -> m ()
        resolveExtern flat = do
            ObjExtern (S.Extern pos sym params retty) <- getObj flat

            pushScope
            params' <- forM params $ \(S.Param pos sym typ) -> do
                checkSymKeyUndef sym KeyVar
                flat <- fresh sym
                addSym sym KeyVar flat
                fmap (S.Param pos flat) (resolveType typ) 
            popScope

            retty' <- case retty of
                Nothing -> return Nothing
                Just t  -> fmap Just (resolveType t)

            addObj flat (ObjExtern (S.Extern pos sym params' retty'))


        resolveFunction :: BoM FlattenState m => FlatSym -> m ()
        resolveFunction flat = do
            ObjFuncDef (S.Func pos sym params retty blk) <- getObj flat

            pushScope
            params' <- forM params $ \(S.Param pos sym typ) -> do
                checkSymKeyUndef sym KeyVar
                flat <- fresh sym
                addSym sym KeyVar flat
                fmap (S.Param pos flat) (resolveType typ)


            blk' <- mapM resolveStmt blk
            popScope

            retty' <- case retty of
                Nothing -> return Nothing
                Just t  -> fmap Just (resolveType t)

            addObj flat (ObjFuncDef (S.Func pos flat params' retty' blk'))


        resolveStmt :: BoM FlattenState m => S.Stmt -> m S.Stmt
        resolveStmt stmt = case stmt of
            S.Assign pos pat expr -> do
                pat' <- resolvePattern pat
                expr' <- resolveExpr expr
                return (S.Assign pos pat' expr')
            S.While pos cnd blk -> do
                pushScope
                cnd' <- resolveExpr cnd
                blk' <- mapM resolveStmt blk
                popScope
                return (S.While pos cnd' blk')
            S.Switch pos cnd cases -> do
                cnd' <- resolveExpr cnd
                pushScope
                cases' <- forM cases $ \(pat, stmt) -> do
                    pat' <- resolvePattern pat
                    stmt' <- resolveStmt stmt
                    return (pat', stmt')
                popScope
                return (S.Switch pos cnd' cases')
            S.Set pos ind expr -> do
                ind' <- resolveIndex ind
                expr' <- resolveExpr expr
                return (S.Set pos ind' expr')
            S.Return pos mexpr -> do
                fmap (S.Return pos) $ case mexpr of
                    Nothing -> return Nothing
                    Just ex -> fmap Just (resolveExpr ex)
            S.CallStmt pos sym exprs -> do
                flat <- look sym KeyFunc
                exprs' <- mapM resolveExpr exprs
                return (S.CallStmt pos flat exprs')
            S.Print pos exprs -> do
                exprs' <- mapM resolveExpr exprs
                return (S.Print pos exprs')
            _ -> fail ("resolveStmt: " ++ show stmt)


        resolveExpr :: BoM FlattenState m => S.Expr -> m S.Expr
        resolveExpr expr = case expr of
            S.Cons _ -> return expr
            S.Conv pos typ exprs -> do
                typ' <- resolveType typ
                exprs' <- mapM resolveExpr exprs
                return (S.Conv pos typ' exprs')
            S.Ident pos sym ->
                fmap (S.Ident pos) (look sym KeyVar)
            S.Call pos sym exprs -> do
                flat <- look sym KeyFunc
                exprs' <- mapM resolveExpr exprs
                return (S.Call pos flat exprs)
            S.Append pos exprA exprB -> do
                exprA' <- resolveExpr exprA
                exprB' <- resolveExpr exprB
                return (S.Append pos exprA' exprB')
            S.Infix pos op exprA exprB -> do
                exprA' <- resolveExpr exprA
                exprB' <- resolveExpr exprB
                return (S.Infix pos op exprA' exprB')
            S.Len pos expr ->
                fmap (S.Len pos) (resolveExpr expr)
            S.Subscript pos exprA exprB -> do
                exprA' <- resolveExpr exprA
                exprB' <- resolveExpr exprB
                return (S.Subscript pos exprA' exprB')
            _ -> fail ("resolveExpr: " ++ show expr)


        resolveType :: BoM FlattenState m => T.Type -> m T.Type
        resolveType typ = case typ of
            T.I8        -> return T.I8
            T.Bool      -> return T.Bool
            T.Char      -> return T.Char
            T.Typedef s -> fmap T.Typedef (look s KeyType)
            T.Table ts  -> fmap T.Table (mapM resolveType ts)
            _ -> fail ("resolveTyp: " ++ show typ)


prettyFlatAST :: FlattenState -> IO ()
prettyFlatAST flatAST = do
    putStrLn "objects:"
    forM_ (Map.toList $ defTab flatAST) $ \(flat, obj) ->
        putStrLn $ take 100 ("\t" ++ flat ++ ": " ++ show obj)


