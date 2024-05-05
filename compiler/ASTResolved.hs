module ASTResolved where

import Data.List

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import AST
import Symbol
import Type



data ASTResolved
    = ASTResolved
        { moduleName  :: String
        , includes    :: Set.Set String
        , links       :: Set.Set String
        , imports     :: [ASTResolved]
        , topTypedefs :: [Stmt]
        , topFuncdefs :: [Stmt]
        , topFeatures :: [Stmt]

        -- helping with collect
        , funcHeaders :: Map.Map Symbol FuncHeader

        -- map system to resolved
        , systemSymbols :: Map.Map Symbol Symbol


        , funcInstances :: Map.Map CallHeader Func
        }
    deriving (Eq)







data CallHeader = CallHeader
    { callSymbol    :: Symbol
    , callArgTypes  :: [Type]
    , callRetType   :: Type
    }
    deriving (Eq, Ord)


instance Show CallHeader where
    show header =
        (prettySymbol $ callSymbol header) ++ argsStr ++ ":" ++ show (callRetType header)
        where
            argsStr = "(" ++ intercalate ", " (map show $ callArgTypes header) ++ ")"



callHeaderFromFuncHeader :: FuncHeader -> CallHeader
callHeaderFromFuncHeader (FuncHeader _ _ symbol args retty)
    = CallHeader symbol (map typeof args) (typeof retty)


isGenericFunc :: Func -> Bool
isGenericFunc (Func header _) = isGenericHeader header

isGenericHeader :: FuncHeader -> Bool
isGenericHeader header = funcGenerics header /= []



prettyFuncInstances :: ASTResolved -> IO ()
prettyFuncInstances ast = do
    putStrLn $ "module " ++ moduleName ast
    putStrLn "Instances:"

    forM_ (Map.toList $ funcInstances ast) $ \(call, func) -> do
        putStrLn $ show call
    



prettyASTResolved :: ASTResolved -> IO ()
prettyASTResolved ast = do
    putStrLn $ "module " ++ moduleName ast
    putStrLn ""

    forM_ (topTypedefs ast) $ \stmt -> do
        putStrLn ""
        prettyStmt "" stmt

    forM_ (topFeatures ast) $ \stmt -> do
        putStrLn ""
        prettyStmt "" stmt

    forM_ (topFuncdefs ast) $ \stmt -> do
        putStrLn ""
        prettyStmt "" stmt
