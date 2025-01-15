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
    , fieldsAll      = Map.unions (map fieldsAll imports)
    , instancesAll   = Map.unionsWith Map.union (map instancesAll imports) 
    , funcInstance   = Map.unions (map funcInstance imports)
    , symSupply      = Map.empty
    }


combineAsts :: AstBuilderState -> Map.Map Symbol Int -> [(Import, ASTResolved)] -> DoM s ASTResolved
combineAsts astBuildState supply imports = fmap snd $
    runDoMExcept (initAstResolved (abModuleName astBuildState) (map snd imports)) $ do
        forM_ imports $ \(Import isExport _ _ _, imprt) -> when isExport $ do
            modify $ \s -> s { typeDefsTop = Set.union (typeDefsTop s) (typeDefsTop imprt) }

        modify $ \s -> s
            { includes = Set.fromList [ s | (CInclude s) <- abImports astBuildState ]
            , links    = Set.fromList [ s | (CLink s)    <- abImports astBuildState ]
            , symSupply = supply
            }


        -- define top-level symbols
        needsInfer <- fmap catMaybes $ forM (topStmts astBuildState) $ \topStmt -> case topStmt of
            TopStmt (Typedef _ generics symbol@(SymResolved _) typ) -> do
                modify $ \s -> s
                    { typeDefsTop = Set.insert symbol (typeDefsTop s)
                    , typeDefsAll = Map.insert symbol (generics, typ) (typeDefsAll s)
                    }
                return Nothing

            TopStmt (Enum pos generics symbol fields) -> do
                fieldTypes <- forM fields $ \(SymResolved xs, fieldTypes) -> case fieldTypes of
                    [x] -> return x
                    xs  -> return $ foldType (Tuple : xs)
                let sumType = foldType (Sum : fieldTypes)
                let enmType = foldType (TypeDef symbol : map TypeDef generics)

                forM_ (zip fields [0..]) $ \((fieldSymbol@(SymResolved xs), ts), i) -> do
                    instSymbol <- genSymbol $ SymResolved xs

                    let funcType = foldType (Func : enmType : ts)
                    let acqType = foldType (TypeDef fieldSymbol : map TypeDef generics)

                    modify $ \s -> s
                        { fieldsAll = Map.insert fieldSymbol (symbol, i) (fieldsAll s)
                        -- create constructor function
                        , typeDefsTop = Set.insert fieldSymbol (typeDefsTop s)
                        , typeDefsAll = Map.insert fieldSymbol (generics, funcType) (typeDefsAll s)
                        , instancesAll = Map.insert fieldSymbol (Map.singleton instSymbol $ TopField pos generics acqType i) (instancesAll s)
                        , featuresAll  = Map.insert fieldSymbol (Function pos generics [] fieldSymbol funcType) (featuresAll s)
                        }

                modify $ \s -> s
                    { typeDefsTop = Set.insert symbol (typeDefsTop s)
                    , typeDefsAll = Map.insert symbol (generics, sumType) (typeDefsAll s)
                    }
                return Nothing


            TopStmt stmt@(Function _ generics _ symbol@(SymResolved _) funcType) -> do
                modify $ \s -> s
                    { featuresTop = Set.insert symbol (featuresTop s)
                    , typeDefsTop = Set.insert symbol (typeDefsTop s)
                    , featuresAll = Map.insert symbol stmt (featuresAll s)
                    , typeDefsAll = Map.insert symbol (generics, funcType) (typeDefsAll s)
                    }
                return Nothing


            TopStmt (Derives pos generics typ features) -> do
                forM_ features $ \feature -> do
                    let (TypeDef featureSymbol, _) = unfoldType feature
                    let (TypeDef (SymResolved xs), _) = unfoldType feature

                    symbol' <- genSymbol $ SymResolved $ xs ++ [typeCode feature]
                    let stmt' = TopStmt (Derives pos generics typ [feature])

                    existing <- gets (maybe Map.empty id . Map.lookup featureSymbol . instancesAll)
                    modify $ \s -> s { instancesAll = Map.insert
                        featureSymbol
                        (Map.insert symbol' stmt' existing)
                        (instancesAll s)
                        }
                return Nothing

            TopInst _ _ acqType _ _ _ -> do
                let (TypeDef featureSymbol@(SymResolved xs), _) = unfoldType acqType
                instSymbol <- genSymbol $ SymResolved $ xs ++ [typeCode acqType]

                existing <- gets (maybe Map.empty id . Map.lookup featureSymbol . instancesAll)
                modify $ \s -> s { instancesAll = Map.insert
                    featureSymbol (Map.insert instSymbol topStmt existing) (instancesAll s) }
                return $ Just (featureSymbol, instSymbol)


        -- infer types
        forM_ needsInfer $ \(featureSymbol, symbol) -> do
            TopInst pos generics acqTyp params r instState <- gets $
                (Map.! symbol) . (Map.! featureSymbol) . instancesAll

            -- annotate the call types with the number of t0s needed
            types' <- forM (types instState) $ \typ -> case unfoldType typ of
                (TypeDef symbol, []) | isFuncSymbol symbol -> do
                    Just (generics, _) <- Map.lookup symbol <$> getTypeDefs
                    return $ foldType $ typ : replicate (length generics) (Type 0)
                _ -> return typ

            (Type.Func, retty : argTypes) <- unfoldType <$> baseTypeOf acqTyp
            unless (length argTypes == length params) $ withPos pos $
                fail "invalid number of instance arguments"
            params' <- forM (zip params argTypes) $ \(param, argType) -> case param of
                Param pos symbol typ -> return (Param pos symbol argType)
                RefParam pos symbol typ -> return (RefParam pos symbol argType)

            stmt <- TopInst pos generics acqTyp params r <$>
                infer params' retty (instState { types = types'})
            existing <- gets $ (Map.! featureSymbol) . instancesAll

            modify $ \s -> s { instancesAll = Map.insert featureSymbol (Map.insert symbol stmt existing) (instancesAll s) }


isFuncSymbol :: Symbol -> Bool
isFuncSymbol symbol = isLower $ head (sym symbol)

