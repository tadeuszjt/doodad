{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Flatten where
-- Walks an AST and resolves all symbols into unique names depending on scope.

import Control.Monad.State 
import qualified Data.Set as Set 
import qualified Data.Map as Map 
import qualified AST as S
import qualified Type as T
import Monad
import Error


data FlattenState
    = FlattenState
        { imports    :: [FilePath]
        , typeDefs   :: Map.Map String (TextPos, T.Type)
        , varDefs    :: [S.Stmt]
        , funcDefs   :: [S.Stmt]
        , externDefs :: [S.Stmt]
        }


initFlattenState 
    = FlattenState
        { imports    = []
        , typeDefs   = Map.empty
        , varDefs    = []
        , funcDefs   = []
        , externDefs = []
        }


flattenASTs :: BoM FlattenState m => [S.AST] -> m ()
flattenASTs asts = do
    ast <- combineASTs asts
    flattenAST ast


combineASTs :: BoM s m => [S.AST] -> m S.AST
combineASTs asts = do
    let modNames = Set.toList $ Set.fromList $ map S.astModuleName asts
    assert (length modNames == 1) ("differing module names in asts: " ++ show modNames)

    return S.AST {
        S.astModuleName = head modNames,
        S.astImports    = Set.toList $ Set.fromList $ concat (map S.astImports asts),
        S.astStmts      = concat (map S.astStmts asts)
        }
        

flattenAST :: BoM FlattenState m => S.AST -> m ()
flattenAST ast = do
    mapM_ gatherTopStmt (S.astStmts ast)
    mapM_ checkTypedefCircles . Map.keys =<< gets typeDefs
    modify $ \s -> s { imports = S.astImports ast }

    modify $ \s -> s {
        funcDefs   = reverse (funcDefs s),
        externDefs = reverse (externDefs s),
        varDefs    = reverse (varDefs s)
        }
    where
        moduleName = maybe "main" id (S.astModuleName ast)
        
        gatherTopStmt :: BoM FlattenState m => S.Stmt -> m ()
        gatherTopStmt stmt = case stmt of
            S.FuncDef _ _ _ _ _   -> modify $ \s -> s { funcDefs   = stmt:(funcDefs s) }
            S.Extern _ _ _ _ _    -> modify $ \s -> s { externDefs = stmt:(externDefs s) }
            S.Assign _ _ _        -> modify $ \s -> s { varDefs    = stmt:(varDefs s) }
            S.Typedef pos (T.Sym sym) typ -> do
                b <- Map.member sym <$> gets typeDefs
                assert (not b) (sym ++ " already defined")
                modify $ \s -> s { typeDefs = Map.insert sym (pos, typ) (typeDefs s) }

            _ -> fail "invalid top-level statement"

        checkTypedefCircles :: BoM FlattenState m => String -> m ()
        checkTypedefCircles sym = do
            checkTypedefCircles' sym Set.empty
            where
                checkTypedefCircles' :: BoM FlattenState m => String -> Set.Set String -> m ()
                checkTypedefCircles' sym visited = do
                    assert (not $ Set.member sym visited) ("circular type dependency: " ++ sym)
                    res <- Map.lookup sym <$> gets typeDefs
                    case res of
                        Just (pos, T.Typedef (T.Sym s)) -> checkTypedefCircles' s (Set.insert sym visited)
                        _                               -> return ()


prettyFlatAST :: FlattenState -> IO ()
prettyFlatAST flatAST = do
    putStrLn "typeDefs:"
    forM_ (Map.toList $ typeDefs flatAST) $ \(flat, obj) ->
        putStrLn $ take 100 ("\t" ++ flat ++ ": " ++ show obj)
