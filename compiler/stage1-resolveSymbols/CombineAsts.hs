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

 
initAstResolved modName imports = ASTResolved
    { moduleName     = modName
    , includes       = Set.empty
    , links          = Set.empty
    , featuresAll    = Map.unions (map featuresAll imports)
    , typeDefsAll    = Map.unions (map typeDefsAll imports)
    , funcDefsAll    = Map.unions (map funcDefsAll imports)
    , featuresTop    = Set.empty
    , typeDefs       = Set.empty
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

            stmts' <- mapM (mapStmtM combineMapper) (astStmts ast)

            forM_ stmts' $ \stmt -> withPos stmt $ case stmt of
                Typedef pos generics symbol@(SymResolved _) typ ->
                    modify $ \s -> s { typeDefs = Set.insert symbol (typeDefs s) }

                FuncDef (Func header stmt) ->
                    modify $ \s -> s { funcDefsTop = Set.insert (funcSymbol header) (funcDefsTop s) }

                Feature _ _ _ -> return ()



combineMapper :: Elem -> DoM ASTResolved Elem
combineMapper element = case element of
    ElemStmt (FuncDef (Func header stmt)) -> do
        let callHeader = callHeaderFromFuncHeader header
        modify $ \s -> s {
            funcDefsAll = Map.insert (funcSymbol header) (Func header stmt) (funcDefsAll s) }

        when (not $ isGenericHeader header) $ do
            instanceSymbol <- genSymbol $ SymResolved ["instance_" ++ sym (funcSymbol header)]
            let headerInstance = header { funcSymbol = instanceSymbol }
            modify $ \s -> s
                { funcInstance = Map.insert callHeader (Func headerInstance stmt) (funcInstance s)
                } 

        return $ ElemStmt $ FuncDef (Func header stmt)


    ElemStmt (Typedef pos generics symbol typ) -> do
        modify $ \s -> s { typeDefsAll = Map.insert symbol (generics, typ) (typeDefsAll s) }
        return $ ElemStmt (Typedef pos generics symbol typ)

    ElemStmt (Block stmts) -> do
        -- filter out statements
        stmts' <- fmap catMaybes $ forM stmts $ \stmt -> case stmt of
            Typedef _ _ _ _ -> return Nothing
            FuncDef _       -> return Nothing
            Feature _ _ _   -> return Nothing
            _ -> return (Just stmt)

        return $ ElemStmt (Block stmts')


    ElemExpr (Call pos symbol@(Sym [str]) exprs) -> do
        let list = [ "builtin_table_append"
                   , "builtin_table_at"
                   , "builtin_table_slice"
                   , "builtin_slice_at"
                   , "builtin_array_at"
                   , "builtin_zero"
                   , "conv"
                   , "assert"
                   ]
        if str `elem` list then do
            return $ ElemExpr (Builtin pos str exprs)
        else
            return element



    _ -> return element

