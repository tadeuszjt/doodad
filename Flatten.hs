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
    = ObjTypeDef TextPos T.Type
    | ObjVarDef  TextPos S.Expr
    | ObjFuncDef S.Stmt
    | ObjExtern  S.Stmt
    deriving (Show)


data FlattenState
    = FlattenState
        { imports    :: Set.Set S.ModuleName
        , typeDefs   :: Map.Map FlatSym (TextPos, T.Type)
        , varDefs    :: [S.Stmt]
        , funcDefs   :: [S.Stmt]
        , externDefs :: [S.Stmt]
        }


initFlattenState importFlatMap
    = FlattenState
        { imports    = Set.empty
        , typeDefs   = Map.empty
        , varDefs    = []
        , funcDefs   = []
        , externDefs = []
        }


flattenAST
    :: (MonadIO m, MonadFail m)
    => Map.Map S.ModuleName FlattenState
    -> S.AST
    -> m (Either CmpError FlattenState)
flattenAST importFlatMap ast = do
    res <- runBoMT (initFlattenState importFlatMap) $ do
        mapM_ gatherTopStmt (S.astStmts ast)
        mapM_ checkTypedefCircles =<< fmap Map.keys (gets typeDefs)
    case res of
        Left err         -> return (Left err)
        Right (_, state) -> return (Right state { imports = Map.keysSet importFlatMap })
    where
        moduleName = maybe "main" id (S.astModuleName ast)
        
        gatherTopStmt :: BoM FlattenState m => S.Stmt -> m ()
        gatherTopStmt stmt = case stmt of
            S.Typedef pos sym typ -> do
                b <- fmap (Map.member sym) (gets typeDefs)
                when b $ fail (sym ++ " already defined")
                modify $ \s -> s { typeDefs = Map.insert sym (pos, typ) (typeDefs s) }
            S.Func pos sym params retty blk -> do
                modify $ \s -> s { funcDefs = stmt:(funcDefs s) }
            S.Extern pos sym params retty -> do
                modify $ \s -> s { externDefs = stmt:(externDefs s) }
            S.Assign pos (S.PatIdent p sym) expr -> do
                modify $ \s -> s { varDefs = stmt:(varDefs s) }
            _ -> fail "invalid top-level statement"


        checkTypedefCircles :: BoM FlattenState m => FlatSym -> m ()
        checkTypedefCircles flat = do
            checkTypedefCircles' flat Set.empty
            where
                checkTypedefCircles' :: BoM FlattenState m => FlatSym -> Set.Set FlatSym -> m ()
                checkTypedefCircles' flat visited = do
                    when (Set.member flat visited) $
                        fail ("circular type dependency: " ++ flat)
                    res <- fmap (Map.lookup flat) (gets typeDefs)
                    case res of
                        Just (pos, T.Typedef f) -> checkTypedefCircles' f (Set.insert flat visited)
                        _                       -> return ()




prettyFlatAST :: FlattenState -> IO ()
prettyFlatAST flatAST = do
    putStrLn "typeDefs:"
    forM_ (Map.toList $ typeDefs flatAST) $ \(flat, obj) ->
        putStrLn $ take 100 ("\t" ++ flat ++ ": " ++ show obj)


