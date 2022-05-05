{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Flatten where
-- Walks an AST and resolves all symbols into unique names depending on scope.

import Control.Monad.State 
import qualified Data.Set as Set 
import qualified Data.Map as Map 
import Data.List
import qualified AST as S
import qualified Type as T
import Monad
import Error



-- check typedefs for circles
checkTypeDefs :: BoM s m => [S.Stmt] -> m ()
checkTypeDefs typedefs = do
    -- check multiple definitions
    forM typedefs $ \(S.Typedef pos sym anno) -> withPos pos $
        case elemIndices sym (map typedefSym typedefs) of
            [x] -> return ()
            _   -> fail $ "multiple definitions of " ++ sym

    -- check circles
    mapM_ (checkCircles Set.empty) (map typedefSym typedefs)

    where
        typedefSym :: S.Stmt -> String
        typedefSym (S.Typedef _ sym _) = sym

        checkCircles :: BoM s m => Set.Set String -> String -> m ()
        checkCircles visited sym = case elemIndices sym (map typedefSym typedefs) of
            [] -> return ()
            [idx] -> do
                let S.Typedef pos _ anno = typedefs !! idx
                withPos pos $ assert (not $ Set.member sym visited) "Typedef has circles"
                checkAnnoCircles (Set.insert sym visited) anno
        
        checkAnnoCircles :: BoM s m => Set.Set String -> S.AnnoType -> m ()
        checkAnnoCircles visited anno = case anno of
            S.AnnoType t   -> checkTypeCircles visited t
            S.AnnoTuple xs -> forM_ xs $ \(_, t) -> checkTypeCircles visited t
            _              -> fail (show anno)

        checkTypeCircles :: BoM s m => Set.Set String -> T.Type -> m ()
        checkTypeCircles visited typ = case typ of
            T.Typedef (T.Sym s) -> checkCircles visited s
            T.Tuple ts          -> mapM_ (checkTypeCircles visited) ts
            t | T.isSimple t    -> return ()
            _                   -> fail ("checkTypeCircles " ++ show typ)



data FlattenState
    = FlattenState
        { varDefs    :: [S.Stmt]
        , funcDefs   :: [S.Stmt]
        , externDefs :: [S.Stmt]
        , typedefs   :: [S.Stmt]
        }


initFlattenState 
    = FlattenState
        { varDefs    = []
        , funcDefs   = []
        , externDefs = []
        , typedefs   = []
        }


flattenASTs :: BoM FlattenState m => [S.AST] -> m S.AST
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
        

flattenAST :: BoM FlattenState m => S.AST -> m S.AST
flattenAST ast = do
    mapM_ gatherTopStmt (S.astStmts ast)
    checkTypeDefs [ stmt | stmt@(S.Typedef _ _ _) <- S.astStmts ast ]

    modify $ \s -> s {
        funcDefs   = reverse (funcDefs s),
        externDefs = reverse (externDefs s),
        varDefs    = reverse (varDefs s),
        typedefs   = reverse (typedefs s)
        }

    s <- get

    return $ ast { S.astStmts = typedefs s ++ externDefs s ++ varDefs s ++ funcDefs s }
    where
        moduleName = maybe "main" id (S.astModuleName ast)
        
        gatherTopStmt :: BoM FlattenState m => S.Stmt -> m ()
        gatherTopStmt stmt = case stmt of
            S.FuncDef _ _ _ _ _        -> modify $ \s -> s { funcDefs   = stmt:(funcDefs s) }
            S.Extern _ _ _ _ _         -> modify $ \s -> s { externDefs = stmt:(externDefs s) }
            S.ExternVar _ _ _ _        -> modify $ \s -> s { externDefs = stmt:(externDefs s) }
            S.Assign _ _ _             -> modify $ \s -> s { varDefs    = stmt:(varDefs s) }
            S.Typedef pos sym annoType -> modify $ \s -> s { typedefs   = stmt:(typedefs s) }
            _ -> fail "invalid top-level statement"

