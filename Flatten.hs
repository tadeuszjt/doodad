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

data FlattenState
    = FlattenState
        { importFlat :: Map.Map S.ModuleName FlattenState
        , typedefs   :: Map.Map FlatSym (TextPos, T.Type)
        , variables  :: Map.Map FlatSym (TextPos, S.Expr)
        , funcDefs   :: Map.Map FlatSym S.Stmt
        , externs    :: Map.Map FlatSym S.Stmt
        , symTab     :: SymTab.SymTab S.Symbol FlatSym
        , symSupply  :: Map.Map S.Symbol Int
        }

initFlattenState importFlatMap
    = FlattenState
        { importFlat = importFlatMap
        , typedefs   = Map.empty
        , variables  = Map.empty
        , funcDefs   = Map.empty
        , externs    = Map.empty
        , symTab     = SymTab.initSymTab
        , symSupply  = Map.empty
        }


fresh :: BoM FlattenState m => S.Symbol -> m FlatSym
fresh sym = do
    res <- fmap (Map.lookup sym) (gets symSupply)
    let i = maybe 0 (+1) res
    modify $ \s -> s { symSupply = Map.insert sym i (symSupply s) }
    return (sym ++ "_" ++ show i)


checkSymUndefined :: BoM FlattenState m => S.Symbol -> m ()
checkSymUndefined sym = do
    res <- fmap (SymTab.lookupHead sym) (gets symTab)
    when (isJust res) $ fail (sym ++ " already defined")


lookImportSym :: BoM FlattenState m => S.Symbol -> m FlatSym
lookImportSym sym = do
    importFlatMap <- (gets importFlat)
    let symTabs    = map symTab $ Map.elems importFlatMap
    let symTabTops = map (\[ss] -> ss) symTabs
    let ress       = map fromJust $ filter isJust $ map (Map.lookup sym) symTabTops
    when (length ress /= 1) $ fail (sym ++ " not defined once")
    return (head ress)
        

lookSym :: BoM FlattenState m => S.Symbol -> m FlatSym
lookSym sym = do
    res <- fmap (SymTab.lookup sym) (gets symTab)
    case res of
        Just r  -> return r
        Nothing -> lookImportSym sym

addSym :: BoM FlattenState m => S.Symbol -> FlatSym -> m ()
addSym sym flat =
    modify $ \s -> s { symTab = SymTab.insert sym flat (symTab s) }


pushScope :: BoM FlattenState m => m ()
pushScope =
    modify $ \s -> s { symTab = SymTab.push (symTab s) }

popScope :: BoM FlattenState m => m ()
popScope =
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


flattenAST
    :: (MonadIO m, MonadFail m)
    => Map.Map S.ModuleName FlattenState
    -> S.AST
    -> m (Either CmpError FlattenState)
flattenAST importFlatMap ast = do
    res <- runBoMT (initFlattenState importFlatMap) $ do
        mapM_ flattenTopStmt (S.astStmts ast)
        mapM_ resolveTypedef =<< fmap Map.keys (gets typedefs)
        mapM_ resolveVariable =<< fmap Map.keys (gets variables)
        mapM_ resolveExtern =<< fmap Map.keys (gets externs)
        mapM_ resolveFunction =<< fmap Map.keys (gets funcDefs)
    case res of
        Left err         -> return (Left err)
        Right (_, state) -> return (Right state)
    where
        moduleName = maybe "main" id (S.astModuleName ast)
        
        flattenTopStmt :: BoM FlattenState m => S.Stmt -> m ()
        flattenTopStmt stmt = case stmt of
            S.Typedef pos sym typ -> do
                checkSymUndefined sym
                flatSym <- fresh (moduleName ++ "_type_" ++ sym)
                addSym sym flatSym
                modify $ \s -> s { typedefs = Map.insert flatSym (pos, typ) (typedefs s) }
            S.Func pos sym params retty blk -> do
                checkSymUndefined sym
                flatSym <- fresh (moduleName ++ "_fn_" ++ sym)
                addSym sym flatSym
                modify $ \s -> s { funcDefs = Map.insert flatSym stmt (funcDefs s) }
            S.Extern pos sym params retty -> do
                checkSymUndefined sym
                addSym sym sym
                modify $ \s -> s { externs = Map.insert sym stmt (externs s) }
            S.Assign pos (S.PatIgnore _) expr ->
                return ()
            S.Assign pos (S.PatIdent p sym) expr -> do
                checkSymUndefined sym
                flatSym <- fresh (moduleName ++ "_var_" ++ sym)
                addSym sym flatSym
                expr' <- resolveConstExpr expr
                modify $ \s -> s { variables = Map.insert flatSym (pos, expr') (variables s) }
            _ -> fail "invalid top-level statement"

        resolveTypedef :: BoM FlattenState m => FlatSym -> m ()
        resolveTypedef flatSym = do
            (pos, typ) <- resolveTypedef' Set.empty flatSym
            modify $ \s -> s { typedefs = Map.insert flatSym (pos, typ) (typedefs s) }
            where
                resolveTypedef' :: BoM FlattenState m => Set.Set FlatSym -> FlatSym -> m (TextPos, T.Type)
                resolveTypedef' visitedSyms flatSym = do
                    when (Set.member flatSym visitedSyms) $
                        fail ("circular type dependency: " ++ flatSym)

                    (pos, typ) <- fmap (Map.! flatSym) (gets typedefs)

                    case typ of
                        T.Typedef s -> do
                            flatS <- lookSym s
                            resolveTypedef' (Set.insert flatSym visitedSyms) flatS
                            return (pos, T.Typedef flatS)
                        T.Bool ->
                            return (pos, typ)

        resolveVariable :: BoM FlattenState m => FlatSym -> m ()
        resolveVariable flatSym = do
            (pos, expr) <- fmap (Map.! flatSym) (gets variables)
            expr' <- resolveConstExpr expr
            modify $ \s -> s { variables = Map.insert flatSym (pos, expr') (variables s) }

        resolveConstExpr :: BoM FlattenState m => S.Expr -> m S.Expr
        resolveConstExpr expr = case expr of
            S.Cons _          -> return expr
            S.Tuple pos exprs -> fmap (S.Tuple pos) (mapM resolveConstExpr exprs)

        resolvePattern :: BoM FlattenState m => S.Pattern -> m S.Pattern
        resolvePattern pat = case pat of
            S.PatIgnore pos    -> return pat
            S.PatIdent pos sym -> do
                checkSymUndefined sym
                flat <- fresh sym
                addSym sym flat
                return (S.PatIdent pos flat)
            S.PatLiteral cons ->
                return pat
            _ -> fail ("resolvePattern: " ++ show pat)

        resolveIndex :: BoM FlattenState m => S.Index -> m S.Index
        resolveIndex ind = case ind of
            S.IndIdent pos sym -> fmap (S.IndIdent pos) (lookSym sym)

        resolveExtern :: BoM FlattenState m => FlatSym -> m ()
        resolveExtern flat = do
            S.Extern pos sym params retty <- fmap (Map.! flat) (gets externs)

            pushScope
            params' <- forM params $ \(S.Param pos sym typ) -> do
                checkSymUndefined sym
                flat <- fresh sym
                addSym sym flat
                fmap (S.Param pos flat) (resolveType typ) 
            popScope

            retty' <- case retty of
                Nothing -> return Nothing
                Just t  -> fmap Just (resolveType t)

            modify $ \s -> s { externs = Map.insert flat (S.Extern pos sym params' retty') (externs s) }

        resolveFunction :: BoM FlattenState m => FlatSym -> m ()
        resolveFunction flat = do
            S.Func pos sym params retty blk <- fmap (Map.! flat) (gets funcDefs)

            pushScope
            params' <- forM params $ \(S.Param pos sym typ) -> do
                checkSymUndefined sym
                flat <- fresh sym
                addSym sym flat
                fmap (S.Param pos flat) (resolveType typ)


            blk' <- mapM resolveStmt blk
            popScope

            retty' <- case retty of
                Nothing -> return Nothing
                Just t  -> fmap Just (resolveType t)

            modify $ \s -> s { funcDefs = Map.insert flat (S.Func pos flat params' retty' blk') (funcDefs s) }

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
                flat <- lookSym sym
                exprs' <- mapM resolveExpr exprs
                return (S.CallStmt pos flat exprs')
            _ -> fail ("resolveStmt: " ++ show stmt)

        resolveExpr :: BoM FlattenState m => S.Expr -> m S.Expr
        resolveExpr expr = case expr of
            S.Cons _ -> return expr
            S.Conv pos typ exprs -> do
                typ' <- resolveType typ
                exprs' <- mapM resolveExpr exprs
                return (S.Conv pos typ' exprs')
            S.Ident pos sym ->
                fmap (S.Ident pos) (lookSym sym)
            S.Call pos sym exprs -> do
                flat <- lookSym sym
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
            T.Typedef s -> fmap T.Typedef (lookSym s)
            T.Table ts  -> fmap T.Table (mapM resolveType ts)
            _ -> fail ("resolveTyp: " ++ show typ)


prettyFlatAST :: FlattenState -> IO ()
prettyFlatAST flatAST = do
    putStrLn "Typedefs:"
    forM_ (Map.toList $ typedefs flatAST) $ \typedef -> putStrLn ("\t" ++ show typedef)
    putStrLn "Variables:"
    forM_ (Map.toList $ variables flatAST) $ \(sym, var) -> putStrLn ("\t" ++ sym ++ " " ++ show var)
    putStrLn "Externs:"
    forM_ (Map.toList $ externs flatAST) $ \(sym, (S.Extern pos name params retty)) ->
        putStrLn $ "\t" ++ sym ++ " " ++ (show params) ++ " " ++ show retty
    putStrLn "Functions:"
    forM_ (Map.toList $ funcDefs flatAST) $ \(flatSym, (S.Func pos name params retty blk)) -> do
        putStrLn $ "\t" ++ flatSym ++ " " ++ (show params) ++ " " ++ show retty
        S.prettyAST "\t\t" $ S.AST {
            S.astStmts = blk,
            S.astModuleName = Nothing,
            S.astImports = Set.empty
            }

