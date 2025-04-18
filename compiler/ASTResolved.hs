{-# LANGUAGE FlexibleContexts #-}
module ASTResolved where

import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import AST
import Symbol
import Type
import AstBuilder

import qualified Ir


data ASTResolved
    = ASTResolved
        { moduleName           :: String
        , includes             :: Set.Set String           -- c header includes
        , links                :: Set.Set String           -- linked libraries

        , typeDefsAll          :: Type.TypeDefsMap         -- all type defs
        , typeDefsTop          :: Set.Set Symbol           -- top-level type defs

        , fieldsAll            :: Map.Map Symbol (Symbol, Int)

        , featuresTop          :: Set.Set Symbol
        , featuresAll          :: Map.Map Symbol Stmt
        
        , instancesTop         :: Set.Set Symbol
        , instancesAll         :: Map.Map Symbol TopStmt
        
        , instantiationsTop    :: Set.Set Type
        , instantiations       :: Map.Map Type Ir.FuncIr

        , symSupply            :: Map.Map Symbol Int              
        }


-- works assuming unique module name
genSymbol :: MonadState ASTResolved m => Symbol -> m Symbol
genSymbol symbol@(SymResolved str) = do  
    modName <- gets moduleName
    im <- gets $ Map.lookup symbol . symSupply
    let n = maybe 0 (id) im
    modify $ \s -> s { symSupply = Map.insert symbol (n + 1) (symSupply s) }
    case n of
        0 -> return $ SymResolved ([modName] ++ str)
        n -> return $ SymResolved ([modName] ++ str ++ [show n])


findSymbol :: MonadState ASTResolved m => Symbol -> m Symbol
findSymbol symbol = do
    xs <- filter (symbolsCouldMatch symbol) . Map.keys . typeDefsAll <$> get
    case xs of
        [] -> error $ "symbol not found: " ++ prettySymbol symbol
        [x] -> return x
        xs  -> error "multiple symbols"


prettyASTResolved :: ASTResolved -> IO ()
prettyASTResolved ast = do
    putStrLn $ "module " ++ moduleName ast
    forM_ (includes ast) $ \str -> putStrLn $ "#include " ++ show str
    forM_ (links ast) $ \str -> putStrLn $ "link " ++ str


    putStrLn ""
    putStrLn "typeDefs:"
    forM_ (Set.toList $ typeDefsTop ast) $ \symbol -> do
        let (generics, typ) = typeDefsAll ast Map.! symbol
        liftIO $ putStrLn $
                "\t"
                ++ brcStrs (map prettySymbol generics)
                ++ " "
                ++ prettySymbol symbol
                ++ " = "
                ++ show typ

    putStrLn ""
    putStrLn "features:"
    forM_ (Set.toList $ typeDefsTop ast) $ \symbol -> do
        liftIO $ putStrLn $ "\t" ++ prettySymbol symbol


    putStrLn ""
    putStrLn "instancesAll:"
    forM_ (instancesTop ast) $ \symbol -> do
        let topStmt = instancesAll ast Map.! symbol
        liftIO $ putStrLn $ "\t" ++ prettySymbol symbol


printAstIr :: ASTResolved -> IO ()
printAstIr ast = do
    putStrLn $ "module " ++ moduleName ast
    
    forM_ (instantiationsTop ast) $ \callType -> do
        let Just funcIr = Map.lookup callType (instantiations ast)
        putStrLn ""
        putStrLn $ show (Ir.irContexts funcIr) 
        putStrLn $ show callType ++ " " ++ show (Ir.irArgs funcIr)
        Ir.prettyFuncIr "\t" funcIr
