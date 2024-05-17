module CombineAsts where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

import ASTResolved
import ASTMapper
import AST
import Monad
import Symbol
import Error
import Type

 
initAstResolved modName imports = ASTResolved
    { moduleName     = modName
    , includes       = Set.empty
    , links          = Set.empty
    , typeDefsAll    = Map.unions (map typeDefsAll imports)
    , funcDefsAll    = Map.unions (map funcDefsAll imports)

    , featureDefsAll   = Map.unions (map featureDefsAll imports)
    , featureDefsTop   = Set.empty

    , aquiresAll     = Map.unions (map aquiresAll imports) 
    , aquiresTop     = Set.empty

    , typeDefsTop    = Set.empty

    , funcDefsTop    = Set.empty
    , funcInstance   = Map.empty
    , funcInstanceImported = Map.unions $
        (map funcInstance imports) ++ (map funcInstanceImported imports)
    , symSupply      = Map.empty
    }


combineAsts :: (AST, Map.Map Symbol Int) -> [ASTResolved] -> DoM s ASTResolved
combineAsts (ast, supply) imports = fmap snd $
    runDoMExcept (initAstResolved (astModuleName ast) imports) combineAsts'
    where
        combineAsts' :: DoM ASTResolved ()
        combineAsts' = do
            modify $ \s -> s
                { includes = Set.fromList [ s | (CInclude s) <- astImports ast ]
                , links    = Set.fromList [ s | (CLink s)    <- astImports ast ]
                , symSupply = supply
                }

            forM_ (astStmts ast) $ \stmt -> withPos stmt $ case stmt of
                Typedef pos generics symbol@(SymResolved _) typ ->
                    modify $ \s -> s { typeDefsTop = Set.insert symbol (typeDefsTop s) }

                FuncDef generics (AST.Func header stmt) ->
                    modify $ \s -> s { funcDefsTop = Set.insert (funcSymbol header) (funcDefsTop s) }

                Feature _ _ symbol _ _ ->
                    modify $ \s -> s { featureDefsTop = Set.insert symbol (featureDefsTop s) }
                
                Aquires _ _ (Apply (TypeDef symbol) _) _ _ _ -> return ()
                    --modify $ \s -> s { aquiresTop = Set.insert symbol (aquiresTop s) }

            mapM_ (mapStmtM combineMapper) (astStmts ast)

-- TODO this makes stuff ordered FIX 
combineMapper :: Elem -> DoM ASTResolved Elem
combineMapper element = case element of
    ElemStmt (FuncDef generics (AST.Func header stmt)) -> do
        modify $ \s -> s { funcDefsAll = Map.insert (funcSymbol header) (AST.Func header stmt) (funcDefsAll s) }
        modify $ \s -> s { typeDefsAll = Map.insert (funcSymbol header) (generics, typeof header) (typeDefsAll s) }
        return element

    ElemStmt (Typedef pos generics symbol typ) -> do
        modify $ \s -> s { typeDefsAll = Map.insert symbol (generics, typ) (typeDefsAll s) }
        return element

    ElemStmt (Feature pos generics symbol args retty) -> do
        --modify $ \s -> s { featureDefsAll = Map.insert symbol (arg, headers) (featureDefsAll s) }
        modify $ \s -> s { typeDefsAll = Map.insert symbol (generics, Apply Type.Func (retty : args)) (typeDefsAll s) }
        return element

    ElemStmt stmt@(Aquires _ _ typ args _ _) -> do
        symbol <- genSymbol $ SymResolved ["aquires"]
        modify $ \s -> s { aquiresAll = Map.insert symbol stmt (aquiresAll s) }
        modify $ \s -> s { aquiresTop = Set.insert symbol (aquiresTop s) }
        return element

    -- filter out statements
    ElemStmt (Block stmts) -> fmap (ElemStmt . Block . catMaybes) $
        forM stmts $ \stmt -> case stmt of
            Typedef _ _ _ _ -> return Nothing
            FuncDef _ _     -> return Nothing
            Feature _ _ _ _ _ -> return Nothing
            _               -> return (Just stmt)

    ElemExpr (Call pos typ@(TypeDef symbol) exprs) -> do
        resm <- Map.lookup symbol <$> getTypeDefs
        case resm of
            Nothing -> fail ("no def for: " ++ prettySymbol symbol)
            Just (generics, _) -> do
                let typ' = case generics of
                        [] -> typ
                        x  -> Apply typ $ replicate (length x) (Type 0)

                return $ ElemExpr (Call pos typ' exprs)
        

    _ -> return element

