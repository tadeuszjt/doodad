{-# LANGUAGE FlexibleInstances #-}
module ASTResolved where

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
        , includes             :: Set.Set String           -- c header includes
        , links                :: Set.Set String           -- linked libraries

        , typeDefsAll          :: Type.TypeDefsMap         -- all type defs
        , typeDefsTop          :: Set.Set Symbol           -- top-level type defs

        , featuresTop          :: Set.Set Symbol
        , featuresAll          :: Map.Map Symbol Stmt

        , acquiresAll          :: Map.Map Symbol Stmt 
        , acquiresTop          :: Set.Set Symbol

        , funcInstance         :: Map.Map Type FuncHeader

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
    case n of
        0 -> return $ SymResolved ([modName] ++ str)
        n -> return $ SymResolved ([modName] ++ str ++ [show n])


prettyASTResolved :: ASTResolved -> IO ()
prettyASTResolved ast = do
    putStrLn $ "module " ++ moduleName ast

    forM_ (includes ast) $ \str -> putStrLn $ "#include " ++ show str
    forM_ (links ast) $ \str -> putStrLn $ "link " ++ str


    putStrLn ""
    putStrLn "typeDefsTop:"
    forM_ (Set.toList $ typeDefsTop ast) $ \symbol -> do
        liftIO $ putStrLn $ "\t" ++ prettySymbol symbol
    putStrLn "typeDefsAll:"
    forM_ (Map.toList $ typeDefsAll ast) $ \(symbol, (generics, typ)) -> do
        prettyStmt "\t" $ Typedef undefined generics symbol typ


    putStrLn ""
    putStrLn "acquiresTop:"
    forM_ (Set.toList $ acquiresTop ast) $ \symbol -> do
        liftIO $ putStrLn $ "\t" ++ prettySymbol symbol
    putStrLn "acquiresAll:"
    forM_ (Map.toList $ acquiresAll ast) $ \(symbol, acquires) -> do
        prettyStmt "" acquires


--    putStrLn ""
--    putStrLn "funcInstance:"
--    forM_ (Map.toList $ funcInstance ast) $ \(call, func) -> do
--        putStrLn $ show call ++ ":"
--        prettyStmt "\t" $ FuncDef [] func
--
--    putStrLn ""
--    putStrLn "funcInstanceImported:"
--    forM_ (Map.toList $ funcInstanceImported ast) $ \(call, func) -> do
--        putStr $ "\t" ++ show call ++ ": "
--        prettyStmt "" $ FuncDef [] func
