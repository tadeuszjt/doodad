module ASTResolved where

import Data.List

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import AST
import Symbol
import Type



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
        , symSupply            :: Map.Map String Int              
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
    putStrLn ""

    forM_ (Set.toList $ typeDefs ast) $ \symbol -> do
        let (generics, typ) = typeDefsAll ast Map.! symbol
        prettyStmt "" (AST.Typedef undefined generics symbol $ AnnoType typ)

    putStrLn ""

    forM_ (Set.toList $ funcDefsTop ast) $ \symbol -> do
        let func = funcDefsAll ast Map.! symbol
        prettyStmt "" $ FuncDef (Func (funcHeader func) (funcStmt func))

    putStrLn ""
