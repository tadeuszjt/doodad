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
        , typeFuncs       :: Type.TypeDefsMap                -- defined type functions
        , ctorDefs        :: Map.Map Symbol (Symbol, Int)    -- defined ctors
        , funcImports     :: Map.Map Symbol FuncBody         -- imported funcs
        , funcDefs        :: Map.Map Symbol FuncBody         -- defined functions
        , symSupply       :: Map.Map String Int              -- type supply from resovle
        }
    deriving (Eq)


data CallHeader = CallHeader
    { callParamType :: Maybe Type
    , callSymbol    :: Symbol
    , callArgTypes  :: [Type]
    , callRetType   :: Type
    }
    deriving (Eq, Ord)


data FuncBody
    = FuncBody
        { funcGenerics :: [Symbol]
        , funcParams   :: [AST.Param]
        , funcArgs     :: [AST.Param]
        , funcRetty    :: Type
        , funcStmt     :: AST.Stmt
        }
    deriving (Eq, Show)


instance Show CallHeader where
    show header =
        paramsStr ++ " " ++ (show $ callSymbol header) ++ argsStr ++ " " ++ show (callRetType header)
        where
            paramsStr = case callParamType header of
                Nothing -> ""
                Just t  -> show t ++ "."
            argsStr = case callArgTypes header of
                [] -> "()"
                ts -> "(" ++ intercalate ", " (map show ts) ++ ")"

isGenericBody :: FuncBody -> Bool
isGenericBody (FuncBody [] _ _ _ _) = False
isGenericBody _                     = True


isGenericFunction :: Symbol -> ASTResolved -> Bool
isGenericFunction symbol ast = if Map.member symbol (funcDefs ast) then
        isGenericBody (funcDefs ast Map.! symbol)
    else if Map.member symbol (funcImports ast) then
        isGenericBody (funcImports ast Map.! symbol)
    else False


isNonGenericFunction :: Symbol -> ASTResolved -> Bool
isNonGenericFunction symbol ast = if Map.member symbol (funcDefs ast) then
        not $ isGenericBody (funcDefs ast Map.! symbol)
    else if Map.member symbol (funcImports ast) then
        not $ isGenericBody (funcImports ast Map.! symbol)
    else False


isCtor :: Symbol -> ASTResolved -> Bool
isCtor symbol ast = Map.member symbol (ctorDefs ast)



getTypeFunction :: Symbol -> ASTResolved -> ([Symbol], Type)
getTypeFunction symbol ast = if Map.member symbol (typeFuncs ast) then
        typeFuncs ast Map.! symbol
    else error "symbol is not a type"


getFunctionTypeArgs :: Symbol -> ASTResolved -> [Symbol]
getFunctionTypeArgs symbol ast = if Map.member symbol (funcDefs ast) then
        let body = funcDefs ast Map.! symbol in funcGenerics body
    else if Map.member symbol (funcImports ast) then
        let body = funcImports ast Map.! symbol in funcGenerics body
    else error "symbol is not function"


getFunctionBody :: Symbol -> ASTResolved -> FuncBody
getFunctionBody symbol ast = if Map.member symbol (funcDefs ast) then
        funcDefs ast Map.! symbol
    else if Map.member symbol (funcImports ast) then
        funcImports ast Map.! symbol
    else error "symbol is not function"


funcHeaderTypesMatch :: FuncBody -> FuncBody -> Bool
funcHeaderTypesMatch a b =
    funcRetty a == funcRetty b &&
    map typeof (funcParams a) == map typeof (funcParams b) &&
    map typeof (funcArgs a) == map typeof (funcArgs b)



prettyFuncBody :: Symbol -> FuncBody -> IO ()
prettyFuncBody symbol body =
    prettyStmt "" $ FuncDef
        undefined
        (funcGenerics body)
        (funcParams body)
        symbol
        (funcArgs body)
        (funcRetty body)
        (funcStmt body)


prettyASTResolved :: ASTResolved -> IO ()
prettyASTResolved ast = do
    putStrLn $ "module " ++ moduleName ast
    forM_ (Map.toList $ typeFuncs ast) $ \(symbol, (generics, typ)) ->
        prettyStmt "" (AST.Typedef undefined generics symbol $ AnnoType typ)
    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) -> prettyFuncBody symbol body
    putStrLn ""
