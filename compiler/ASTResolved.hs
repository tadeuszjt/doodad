{-# LANGUAGE FlexibleInstances #-}
module ASTResolved where

import Data.List

import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import AST
import Symbol
import Type
import Monad



data ASTResolved
    = ASTResolved
        { moduleName           :: String
        , includes             :: Set.Set String                  -- c header includes
        , links                :: Set.Set String                  -- linked libraries
        , typeDefsAll          :: Type.TypeDefsMap                -- all type defs
        , typeDefsTop          :: Set.Set Symbol                  -- top-level type defs

        , featureDefsAll       :: Map.Map Symbol (Symbol, [FuncHeader])
        , featureDefsTop       :: Set.Set Symbol

        , funcDefsAll          :: Map.Map Symbol Func
        , funcDefsTop          :: Set.Set Symbol
        , funcInstance         :: Map.Map (Symbol, Type) Func
        , funcInstanceImported :: Map.Map (Symbol, Type) Func
        , symSupply            :: Map.Map Symbol Int              
        }
    deriving (Eq)


instance TypeDefs (DoM ASTResolved) where
    getTypeDefs = gets typeDefsAll


genSymbol :: Symbol -> DoM ASTResolved Symbol
genSymbol symbol@(SymResolved str) = do  
    modName <- gets moduleName
    im <- gets $ Map.lookup symbol . symSupply
    let n = maybe 0 (id) im
    modify $ \s -> s { symSupply = Map.insert symbol (n + 1) (symSupply s) }
    return $ SymResolved ([modName] ++ str ++ [show n])


getFunction :: Symbol -> ASTResolved -> Func
getFunction symbol ast = if Map.member symbol (funcDefsAll ast) then
        funcDefsAll ast Map.! symbol
    else error ("symbol is not function: " ++ prettySymbol symbol)


getFunctionHeader :: Symbol -> ASTResolved -> FuncHeader
getFunctionHeader symbol ast = funcHeader $
    if Map.member symbol (funcDefsAll ast) then
        funcDefsAll ast Map.! symbol
    else error ("symbol is not function: " ++ prettySymbol symbol)


getInstanceHeader :: Symbol -> ASTResolved -> FuncHeader
getInstanceHeader symbol ast = funcHeader $ snd $ head $ Map.toList $ 
    Map.filter (\func -> funcSymbol (funcHeader func) == symbol) allInstances
    where
        allInstances = Map.union (funcInstance ast) (funcInstanceImported ast)


prettyASTResolved :: ASTResolved -> IO ()
prettyASTResolved ast = do
    putStrLn $ "module " ++ moduleName ast

    forM_ (includes ast) $ \str -> putStrLn $ "#include " ++ show str
    forM_ (links ast) $ \str -> putStrLn $ "link " ++ str

    putStrLn ""
    putStrLn "typeDefsAll:"
    forM_ (Map.toList $ typeDefsAll ast) $ \(symbol, (generics, typ)) -> do
        prettyStmt "\t" $ Typedef undefined generics symbol typ

    putStrLn ""
    putStrLn "typeDefs:"
    forM_ (Set.toList $ typeDefsTop ast) $ \symbol -> do
        liftIO $ putStrLn $ "\t" ++ prettySymbol symbol

--    putStrLn ""
--    putStrLn "featuresAll:"
--    forM_ (Map.toList $ featuresAll ast) $ \(symbol, header) -> do
--        liftIO $ putStrLn $ "\t" ++ prettySymbol symbol ++ ": " ++ show header 

    putStrLn ""
    putStrLn "featureDefsTop:"
    forM_ (Set.toList $ featureDefsTop ast) $ \symbol -> do
        putStrLn $ "\t" ++ prettySymbol symbol

    putStrLn ""
    putStrLn "funcDefsAll:"
    forM_ (Map.toList $ funcDefsAll ast) $ \(symbol, func) -> do
        let Just (generics, typ) = Map.lookup symbol (typeDefsAll ast)
        prettyStmt "" $ FuncDef generics func

    putStrLn ""
    putStrLn "funcDefsTop:"
    forM_ (Set.toList $ funcDefsTop ast) $ \symbol -> do
        putStrLn $ "\t" ++ prettySymbol symbol

    putStrLn ""
    putStrLn "funcInstance:"
    forM_ (Map.toList $ funcInstance ast) $ \(call, func) -> do
        putStrLn $ show call ++ ":"
        prettyStmt "\t" $ FuncDef [] func

    putStrLn ""
    putStrLn "funcInstanceImported:"
    forM_ (Map.toList $ funcInstanceImported ast) $ \(call, func) -> do
        putStr $ "\t" ++ show call ++ ": "
        prettyStmt "" $ FuncDef [] func
