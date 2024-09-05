module CombineAsts where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

import ASTResolved
import AST
import Monad
import Symbol
import Error
import Type
import AstBuilder
import InstBuilder

 
initAstResolved modName imports = ASTResolved
    { moduleName     = modName
    , includes       = Set.empty
    , links          = Set.empty

    , typeDefsTop    = Set.empty
    , typeDefsAll    = Map.unions (map typeDefsAll imports)

    , featuresTop    = Set.empty
    , featuresAll    = Map.unions (map featuresAll imports)

    , instancesAll    = Map.unionsWith Map.union (map instancesAll imports) 

    , funcInstance   = Map.unions (map funcInstance imports)

    , symSupply      = Map.empty
    }


combineAsts :: AstBuilderState -> Map.Map Symbol Int -> [(Import, ASTResolved)] -> DoM s ASTResolved
combineAsts astBuildState supply imports = fmap snd $
    runDoMExcept (initAstResolved (abModuleName astBuildState) (map snd imports)) $ do
        forM_ imports $ \(Import isExport _ _ _, imprt) -> when isExport $
            modify $ \s -> s { typeDefsTop = Set.union (typeDefsTop s) (typeDefsTop imprt) }

        modify $ \s -> s
            { includes = Set.fromList [ s | (CInclude s) <- abImports astBuildState ]
            , links    = Set.fromList [ s | (CLink s)    <- abImports astBuildState ]
            , symSupply = supply
            }


        -- define top-level symbols
        forM_ (topStmts astBuildState) $ \topStmt -> case topStmt of
            TopStmt (Typedef _ generics symbol@(SymResolved _) typ) -> do
                modify $ \s -> s { typeDefsTop = Set.insert symbol (typeDefsTop s) }
                modify $ \s -> s { typeDefsAll = Map.insert symbol (generics, typ) (typeDefsAll s) }

            TopStmt stmt@(Feature _ generics _ symbol@(SymResolved _) args retty) -> do
                modify $ \s -> s { featuresTop = Set.insert symbol (featuresTop s) }
                modify $ \s -> s { typeDefsTop = Set.insert symbol (typeDefsTop s) }
                modify $ \s -> s { featuresAll = Map.insert symbol stmt (featuresAll s) }
                modify $ \s -> s { typeDefsAll = Map.insert
                    symbol
                    (generics, foldl Apply Type.Func (retty : args))
                    (typeDefsAll s) }

            TopStmt (Derives pos generics typ features) -> do
                forM_ features $ \feature -> do
                    let (TypeDef featureSymbol, _) = unfoldType feature
                    let (TypeDef (SymResolved xs), _) = unfoldType feature

                    symbol' <- genSymbol $ SymResolved $ xs ++ [typeCode feature]
                    let stmt' = Derives pos generics typ [feature]

                    existing <- gets $ maybe Map.empty id . Map.lookup featureSymbol . instancesAll
                    modify $ \s -> s { instancesAll = Map.insert featureSymbol (Map.insert symbol' stmt' existing) (instancesAll s) }

            _ -> return ()


        forM_ (topStmts astBuildState) $ \topStmt -> case topStmt of
            TopInst p g acqType a r instState -> do
                let (TypeDef featureSymbol, _) = unfoldType acqType
                let (TypeDef (SymResolved xs), _) = unfoldType acqType

                symbol <- genSymbol $ SymResolved $ xs ++ [typeCode acqType]

                expressions' <- forM (expressions instState) $ \expression -> case expression of
                    (Call pos typ exprs) -> do
                        let (TypeDef symbol, _) = unfoldType typ
                        Just (generics, _) <- Map.lookup symbol <$> getTypeDefs
                        let typ' = case unfoldType typ of
                                (TypeDef _, []) -> foldl Apply typ $ replicate (length generics) (Type 0)
                                (TypeDef _, _ ) -> typ

                        return (Call pos typ' exprs)
                    _ -> return expression

                (blk, ()) <- runDoMExcept () $ unbuildInst (instState { expressions = expressions' }) 0
                let stmt = Instance p g acqType a r blk

                resm <- gets $ Map.lookup featureSymbol . instancesAll
                let existing = maybe Map.empty id resm
                modify $ \s -> s { instancesAll = Map.insert featureSymbol (Map.insert symbol stmt existing) (instancesAll s) }

            _ -> return ()
