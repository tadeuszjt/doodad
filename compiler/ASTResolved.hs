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



filterMapBySet :: (Ord k) => Set.Set k -> Map.Map k v -> Map.Map k v
filterMapBySet set map = Map.filterWithKey (\k _ -> Set.member k set) map


data ASTResolved
    = ASTResolved
        { moduleName           :: String
        , includes             :: Set.Set String                  -- c header includes
        , links                :: Set.Set String                  -- linked libraries
        , typeDefsAll          :: Type.TypeDefsMap                -- all type defs
        , typeDefs             :: Set.Set Symbol                  -- top-level type defs

        , featuresAll          :: Map.Map Symbol FuncHeader
        , featuresTop          :: Set.Set Symbol

        , funcDefsAll          :: Map.Map Symbol Func
        , funcDefsTop          :: Set.Set Symbol
        , funcInstance         :: Map.Map CallHeader Func
        , funcInstanceImported :: Map.Map CallHeader Func
        , symSupply            :: Map.Map Symbol Int              
        }
    deriving (Eq)


data CallHeader = CallHeader
    { callSymbol    :: Symbol
    , callArgTypes  :: [Type]
    , callRetType   :: Type
    }
    deriving (Eq, Ord)

instance TypeDefs (DoM ASTResolved) where
    getTypeDefs = gets typeDefsAll


instance Show CallHeader where
    show header =
        (prettySymbol $ callSymbol header) ++ argsStr ++ ":" ++ show (callRetType header)
        where
            argsStr = "(" ++ intercalate ", " (map show $ callArgTypes header) ++ ")"



genSymbol :: Symbol -> DoM ASTResolved Symbol
genSymbol symbol@(SymResolved str) = do  
    modName <- gets moduleName
    im <- gets $ Map.lookup symbol . symSupply
    let n = maybe 0 (id) im
    modify $ \s -> s { symSupply = Map.insert symbol (n + 1) (symSupply s) }
    return $ SymResolved (modName ++ "::" ++ str ++ "::" ++ show n)


callHeaderFromFuncHeader :: FuncHeader -> CallHeader
callHeaderFromFuncHeader (FuncHeader _ _ symbol args retty)
    = CallHeader symbol (map typeof args) (typeof retty)



callCouldMatchFunc :: Monad m => CallHeader -> FuncHeader -> m Bool
callCouldMatchFunc call header = do
    if symbolsMatch then do
        am <- argsMatch
        rm <- rettyMatch
        return (am && rm)
    else return False
    where
        typesMatch :: Monad m => [Type] -> [Type] -> m Bool
        typesMatch ts1 ts2 = do
            bs <- zipWithM typesCouldMatch ts1 ts2
            return $ (length ts1 == length ts2) && (all id bs)

        symbolsMatch = symbolsCouldMatch (callSymbol call) (funcSymbol header)
        argsMatch    = typesMatch (callArgTypes call) (map typeof $ funcArgs header)
        rettyMatch   = typesCouldMatch (callRetType call) (typeof $ funcRetty header)


funcHeaderFullyResolved :: FuncHeader -> Bool
funcHeaderFullyResolved header =
    all typeFullyResolved $ (typeof (funcRetty header) : map typeof (funcArgs header))


isGenericFunc :: Func -> Bool
isGenericFunc (Func header _) = isGenericHeader header

isGenericHeader :: FuncHeader -> Bool
isGenericHeader header = funcGenerics header /= []


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
    forM_ (Set.toList $ typeDefs ast) $ \symbol -> do
        liftIO $ putStrLn $ "\t" ++ prettySymbol symbol

    putStrLn ""
    putStrLn "featuresAll:"
    forM_ (Map.toList $ featuresAll ast) $ \(symbol, header) -> do
        liftIO $ putStrLn $ "\t" ++ prettySymbol symbol ++ ": " ++ show header 

    putStrLn ""
    putStrLn "featuresTop:"
    forM_ (Set.toList $ featuresTop ast) $ \symbol -> do
        putStrLn $ "\t" ++ prettySymbol symbol

    putStrLn ""
    putStrLn "funcDefsAll:"
    forM_ (Map.toList $ funcDefsAll ast) $ \(symbol, func) -> do
        putStr $ "\t" ++ prettySymbol symbol ++ ": "
        prettyStmt "" $ FuncDef func

    putStrLn ""
    putStrLn "funcDefsTop:"
    forM_ (Set.toList $ funcDefsTop ast) $ \symbol -> do
        putStrLn $ "\t" ++ prettySymbol symbol

    putStrLn ""
    putStrLn "funcInstance:"
    forM_ (Map.toList $ funcInstance ast) $ \(call, func) -> do
        putStr $ "\t" ++ show call ++ ": "
        prettyStmt "" $ FuncDef func

    putStrLn ""
    putStrLn "funcInstanceImported:"
    forM_ (Map.toList $ funcInstanceImported ast) $ \(call, func) -> do
        putStr $ "\t" ++ show call ++ ": "
        prettyStmt "" $ FuncDef func
