module CombineAsts where

import Data.Maybe
import Data.Char
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
import InferTypes

 
initAstResolved modName imports = ASTResolved
    { moduleName     = modName
    , includes       = Set.empty
    , links          = Set.empty
    , typeDefsTop    = Set.empty
    , typeDefsAll    = Map.unions (map typeDefsAll imports)
    , featuresTop    = Set.empty
    , featuresAll    = Map.unions (map featuresAll imports)
    , instancesAll   = Map.unionsWith Map.union (map instancesAll imports) 
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

            TopStmt stmt@(Function _ generics _ symbol@(SymResolved _) funcType) -> do
                modify $ \s -> s { featuresTop = Set.insert symbol (featuresTop s) }
                modify $ \s -> s { typeDefsTop = Set.insert symbol (typeDefsTop s) }
                modify $ \s -> s { featuresAll = Map.insert symbol stmt (featuresAll s) }
                modify $ \s -> s { typeDefsAll = Map.insert symbol (generics, funcType) (typeDefsAll s) }

            TopStmt (Derives pos generics typ features) -> do
                forM_ features $ \feature -> do
                    let (TypeDef featureSymbol, _) = unfoldType feature
                    let (TypeDef (SymResolved xs), _) = unfoldType feature

                    symbol' <- genSymbol $ SymResolved $ xs ++ [typeCode feature]
                    let stmt' = TopStmt (Derives pos generics typ [feature])

                    existing <- gets $ maybe Map.empty id . Map.lookup featureSymbol . instancesAll
                    modify $ \s -> s { instancesAll = Map.insert featureSymbol (Map.insert symbol' stmt' existing) (instancesAll s) }

            _ -> return ()


        symbols <- fmap catMaybes $ forM (topStmts astBuildState) $ \topStmt -> case topStmt of
            TopInst p g acqType a r instState -> do
                let (TypeDef featureSymbol, _) = unfoldType acqType
                let (TypeDef (SymResolved xs), _) = unfoldType acqType

                symbol <- genSymbol $ SymResolved $ xs ++ [typeCode acqType]
                let stmt = TopInst p g acqType a r (initInstBuilderState)

                resm <- gets $ Map.lookup featureSymbol . instancesAll
                let existing = maybe Map.empty id resm
                modify $ \s -> s { instancesAll = Map.insert featureSymbol (Map.insert symbol stmt existing) (instancesAll s) }
                return $ Just (featureSymbol, symbol, instState)

            _ -> return Nothing


        forM_ symbols $ \(featureSymbol, symbol, instState) -> do
            (TopInst pos b acqTyp params e _) <- gets $ (Map.! symbol) . (Map.! featureSymbol) . instancesAll

            types' <- forM (types instState) $ \typ -> case unfoldType typ of
                (TypeDef symbol, []) | isFuncSymbol symbol -> do
                    Just (generics, _) <- Map.lookup symbol <$> getTypeDefs
                    return $ foldl Apply typ $ replicate (length generics) (Type 0)
                _ -> return typ

            let inst' = instState { types = types' }

            (Type.Func, retty : argTypes) <- unfoldType <$> baseTypeOf acqTyp

            unless (length argTypes == length params) $ withPos pos $
                fail "invalid number of instance arguments"
            params' <- forM (zip params argTypes) $ \(param, argType) -> case param of
                Param pos symbol typ -> return (Param pos symbol argType)
                RefParam pos symbol typ -> return (RefParam pos symbol argType)

            inst'' <- infer params' retty inst'

            let stmt = TopInst pos b acqTyp params e inst''

            existing <- gets $ (Map.! featureSymbol) . instancesAll
            modify $ \s -> s { instancesAll = Map.insert featureSymbol (Map.insert symbol stmt existing) (instancesAll s) }



isFuncSymbol :: Symbol -> Bool
isFuncSymbol symbol = isLower $ head (sym symbol)

