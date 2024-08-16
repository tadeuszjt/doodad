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

    , typeDefsTop    = Set.empty
    , typeDefsAll    = Map.unions (map typeDefsAll imports)

    , funcDefsAll    = Map.unions (map funcDefsAll imports)
    , funcDefsTop    = Set.empty

    , featuresAll    = Map.unions (map featuresAll imports)

    , fieldsAll      = Map.unions (map fieldsAll imports)

    , acquiresAll    = Map.unions (map acquiresAll imports) 
    , acquiresTop    = Set.empty

    , funcInstance   = Map.unions (map funcInstance imports)

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

            -- define top symbols
            forM_ (astStmts ast) $ \stmt -> withPos stmt $ case stmt of
                Typedef pos generics symbol@(SymResolved _) typ ->
                    modify $ \s -> s { typeDefsTop = Set.insert symbol (typeDefsTop s) }
                FuncDef generics (AST.Func header stmt) -> do
                    modify $ \s -> s { funcDefsTop = Set.insert (funcSymbol header) (funcDefsTop s) }
                    modify $ \s -> s { typeDefsTop = Set.insert (funcSymbol header) (typeDefsTop s) }
                Feature _ _ _ symbol _ _ ->
                    modify $ \s -> s { typeDefsTop = Set.insert symbol (typeDefsTop s) }

                MacroTuple pos generics symbol fields -> do
                    modify $ \s -> s { typeDefsTop = Set.insert symbol (typeDefsTop s) }
                    forM_ fields $ \(fieldSymbol, _) -> do
                        modify $ \s -> s { typeDefsTop = Set.insert fieldSymbol (typeDefsTop s) }

                _ -> return ()
                
            mapM_ (mapStmtM typeDefsMapper) (astStmts ast)
            mapM_ (mapStmtM combineMapper) (astStmts ast)


-- define all typedefs
typeDefsMapper :: Elem -> DoM ASTResolved Elem
typeDefsMapper element = case element of
    ElemStmt (FuncDef generics (AST.Func header _)) -> do
        modify $ \s -> s { typeDefsAll = Map.insert
            (funcSymbol header)
            (generics, typeof header)
            (typeDefsAll s) }
        return element

    ElemStmt (Typedef pos generics symbol typ) -> do
        modify $ \s -> s { typeDefsAll = Map.insert symbol (generics, typ) (typeDefsAll s) }
        return element

    ElemStmt stmt@(Feature pos generics funDeps symbol args retty) -> do
        modify $ \s -> s { featuresAll = Map.insert symbol stmt (featuresAll s) }
        modify $ \s -> s { typeDefsAll = Map.insert
            symbol
            (generics, foldl Apply Type.Func (retty : args))
            (typeDefsAll s) }
        return element

    ElemStmt stmt@(MacroTuple pos generics symbol fields) -> do
        modify $ \s -> s { typeDefsAll = Map.insert
            symbol
            (generics, foldType (Tuple : map snd fields))
            (typeDefsAll s) }

        forM_ (zip fields [0..]) $ \((fieldSymbol, fieldType), i) -> do
            modify $ \s -> s { fieldsAll = Map.insert fieldSymbol (i, symbol) (fieldsAll s) }
            modify $ \s -> s { typeDefsAll = Map.insert
                fieldSymbol
                (generics, foldType [Type.Func, fieldType, foldType (TypeDef symbol : map TypeDef generics)])
                (typeDefsAll s) }

        return element
        

    _ -> return element


combineMapper :: Elem -> DoM ASTResolved Elem
combineMapper element = case element of
    ElemStmt (FuncDef generics (AST.Func header stmt)) -> do
        modify $ \s -> s { funcDefsAll = Map.insert (funcSymbol header) (AST.Func header stmt) (funcDefsAll s) }
        return element

    ElemStmt stmt@(Aquires _ _ _ _ _ _) -> do
        symbol <- genSymbol $ SymResolved ["acquires"]
        modify $ \s -> s { acquiresAll = Map.insert symbol stmt (acquiresAll s) }
        modify $ \s -> s { acquiresTop = Set.insert symbol (acquiresTop s) }
        return element

    ElemStmt stmt@(Derives _ _ _ _) -> do
        symbol' <- genSymbol $ SymResolved ["derives"]
        modify $ \s -> s { acquiresAll = Map.insert symbol' stmt (acquiresAll s) }
        modify $ \s -> s { acquiresTop = Set.insert symbol' (acquiresTop s) }
        return element

    -- filter out statements
    ElemStmt (Block stmts) -> fmap (ElemStmt . Block . catMaybes) $
        forM stmts $ \stmt -> case stmt of
            Typedef _ _ _ _ -> return Nothing
            FuncDef _ _     -> return Nothing
            Feature _ _ _ _ _ _ -> return Nothing
            Aquires _ _ _ _ _ _ -> return Nothing
            Derives _ _ _ _     -> return Nothing
            MacroTuple _ _ _ _  -> return Nothing
            _               -> return (Just stmt)

    ElemExpr (Call pos typ@(TypeDef symbol) exprs) -> do
        resm <- Map.lookup symbol <$> getTypeDefs
        unless (isJust resm) (fail $ "no def for: " ++ prettySymbol symbol)
        let Just (generics, _) = resm

        fieldResm <- Map.lookup symbol <$> gets fieldsAll

        case fieldResm of
            Just (i, _) -> do
                unless (length exprs == 1) (error "cannot have arguments to field access") 
                return $ ElemExpr $ Field pos (exprs !! 0) (Right symbol)
                
            Nothing -> do
                let typ' = case generics of
                        [] -> typ
                        x  -> foldl Apply typ $ replicate (length x) (Type 0)

                return $ ElemExpr (Call pos typ' exprs)
        

    _ -> return element

