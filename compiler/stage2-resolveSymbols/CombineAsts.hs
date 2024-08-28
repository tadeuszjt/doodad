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

    , featuresTop    = Set.empty
    , featuresAll    = Map.unions (map featuresAll imports)

    , acquiresAll    = Map.unions (map acquiresAll imports) 
    , acquiresTop    = Set.empty

    , funcInstance   = Map.unions (map funcInstance imports)

    , symSupply      = Map.empty
    }


combineAsts :: (AST, Map.Map Symbol Int) -> [(Import, ASTResolved)] -> DoM s ASTResolved
combineAsts (ast, supply) imports = fmap snd $
    runDoMExcept (initAstResolved (astModuleName ast) (map snd imports)) combineAsts'
    where
        combineAsts' :: DoM ASTResolved ()
        combineAsts' = do
            forM_ imports $ \(Import isExport _ _ _, imprt) -> when isExport $ do
                modify $ \s -> s { typeDefsTop = Set.union (typeDefsTop s) (typeDefsTop imprt) }

            modify $ \s -> s
                { includes = Set.fromList [ s | (CInclude s) <- astImports ast ]
                , links    = Set.fromList [ s | (CLink s)    <- astImports ast ]
                , symSupply = supply
                }

            -- define top symbols
            forM_ (astStmts ast) $ \stmt -> withPos stmt $ case stmt of
                Typedef pos generics symbol@(SymResolved _) typ ->
                    modify $ \s -> s { typeDefsTop = Set.insert symbol (typeDefsTop s) }
                Feature _ _ _ symbol _ _ ->
                    modify $ \s -> s { typeDefsTop = Set.insert symbol (typeDefsTop s) }

--                MacroTuple pos generics symbol fields -> do
--                    modify $ \s -> s { typeDefsTop = Set.insert symbol (typeDefsTop s) }
--                    forM_ fields $ \(fieldSymbol, _) -> do
--                        modify $ \s -> s { typeDefsTop = Set.insert fieldSymbol (typeDefsTop s) }

                _ -> return ()
                
            mapM_ (mapStmtM typeDefsMapper) (astStmts ast)
            mapM_ (mapStmtM combineMapper) (astStmts ast)


-- define all typedefs
typeDefsMapper :: Elem -> DoM ASTResolved Elem
typeDefsMapper element = case element of
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

    _ -> return element


combineMapper :: Elem -> DoM ASTResolved Elem
combineMapper element = case element of
    ElemStmt stmt@(Acquires _ _ acqType _ _ _) -> do
        let (TypeDef symbol, _) = unfoldType acqType

        symbol <- genSymbol $ symbol
        modify $ \s -> s { acquiresAll = Map.insert symbol stmt (acquiresAll s) }
        modify $ \s -> s { acquiresTop = Set.insert symbol (acquiresTop s) }
        return element

    ElemStmt stmt@(Feature _ _ _ symbol _ _) -> do
        modify $ \s -> s { featuresTop = Set.insert symbol (featuresTop s) }
        return element

    ElemStmt stmt@(Derives pos generics typ ts) -> do
        forM_ ts $ \t -> do
            let (TypeDef symbol, _) = unfoldType typ
            symbol' <- genSymbol symbol
            let stmt' = Derives pos generics typ [t]
            modify $ \s -> s { acquiresAll = Map.insert symbol' stmt' (acquiresAll s) }
            modify $ \s -> s { acquiresTop = Set.insert symbol' (acquiresTop s) }
        return element

    -- filter out statements
    ElemStmt (Block stmts) -> fmap (ElemStmt . Block . catMaybes) $
        forM stmts $ \stmt -> case stmt of
            Typedef _ _ _ _ -> return Nothing
            Feature _ _ _ _ _ _ -> return Nothing
            Acquires _ _ _ _ _ _ -> return Nothing
            Derives _ _ _ _     -> return Nothing
            --MacroTuple _ _ _ _  -> return Nothing
            _               -> return (Just stmt)

    ElemExpr (Call pos typ exprs) -> do
        let (TypeDef symbol, _) = unfoldType typ

        resm <- Map.lookup symbol <$> getTypeDefs
        unless (isJust resm) (fail $ "no def for: " ++ prettySymbol symbol)
        let Just (generics, _) = resm

        let typ' = case (typ, generics) of
                (TypeDef _, []) -> typ
                (TypeDef _, x)  -> foldl Apply typ $ replicate (length x) (Type 0)
                (_, x)          -> typ

        return $ ElemExpr (Call pos typ' exprs)
        

    _ -> return element

