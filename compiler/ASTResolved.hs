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
        { moduleName      :: String
        , includes        :: Set.Set String                  -- c header includes
        , links           :: Set.Set String                  -- linked libraries
        , typeDefsAll     :: Type.TypeDefsMap                -- defined type functions
        , typeDefs        :: Type.TypeDefsMap
        , funcDefs        :: Map.Map Symbol Func         -- defined functions
        , funcImports     :: Map.Map Symbol Func         -- imported funcs
        , funcInstances   :: Map.Map Symbol Func
        , funcExterns     :: Map.Map Symbol Func
        , symSupply       :: Map.Map String Int              -- type supply from resovle
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
        (show $ callSymbol header) ++ argsStr ++ ":" ++ show (callRetType header)
        where
            argsStr = "(" ++ intercalate ", " (map show $ callArgTypes header) ++ ")"


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
getFunction symbol ast = if Map.member symbol (funcDefs ast) then
        funcDefs ast Map.! symbol
    else if Map.member symbol (funcImports ast) then
        funcImports ast Map.! symbol
    else if Map.member symbol (funcInstances ast) then
        funcInstances ast Map.! symbol
    else error ("symbol is not function: " ++ show symbol)


getFunctionHeader :: Symbol -> ASTResolved -> FuncHeader
getFunctionHeader symbol ast = funcHeader $
        if Map.member symbol (funcDefs ast) then
            funcDefs ast Map.! symbol
        else if Map.member symbol (funcImports ast) then
            funcImports ast Map.! symbol
        else if Map.member symbol (funcInstances ast) then
            funcInstances ast Map.! symbol
        else error ("symbol is not function: " ++ show symbol)



prettyASTResolved :: ASTResolved -> IO ()
prettyASTResolved ast = do
    putStrLn $ "module " ++ moduleName ast
    forM_ (Map.toList $ typeDefsAll ast) $ \(symbol, (generics, typ)) ->
        prettyStmt "" (AST.Typedef undefined generics symbol $ AnnoType typ)
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) ->
        prettyStmt "" $ FuncDef (Func (funcHeader func) (funcStmt func))
    putStrLn ""
