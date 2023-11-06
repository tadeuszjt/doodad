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
        , constDefs       :: Map.Map Symbol Expr             -- defined consts
        , typeFuncs       :: Type.TypeDefsMap                -- defined type functions
        , ctorDefs        :: Map.Map Symbol (Symbol, Int)    -- defined ctors
        , funcImports     :: Map.Map Symbol FuncBody         -- imported funcs
        , funcDefs        :: Map.Map Symbol FuncBody         -- defined functions
        , symSupply       :: Map.Map String Int              -- type supply from resovle
        }
    deriving (Eq)


data FuncHeader = FuncHeader
    { typeArgs   :: [Symbol]
    , paramTypes :: [Type]
    , symbol     :: Symbol
    , argTypes   :: [Type]
    , returnType :: Type
    }
    deriving (Eq, Ord)


data FuncBody
    = FuncBodyEmpty
    | FuncBody
        { funcTypeArgs :: [Symbol]
        , funcParams   :: [AST.Param]
        , funcArgs     :: [AST.Param]
        , funcRetty    :: Type
        , funcStmt     :: AST.Stmt
        }
    deriving (Eq, Show)


instance Show FuncHeader where
    show header =
        "fn" ++ typeArgsStr ++ " " ++ paramsStr ++ " " ++ (show $ symbol header) ++ argsStr ++ " " ++ show (returnType header)
        where
            typeArgsStr = case typeArgs header of
                [] -> ""
                ss -> "[" ++ intercalate ", " (map show ss) ++ "]"
            paramsStr = case paramTypes header of
                [] -> ""
                ts -> "{" ++ intercalate ", " (map show ts) ++ "}"
            argsStr = case argTypes header of
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
        let body = funcDefs ast Map.! symbol in funcTypeArgs body
    else if Map.member symbol (funcImports ast) then
        let body = funcImports ast Map.! symbol in funcTypeArgs body
    else error "symbol is not function"


getFunctionHeader :: Symbol -> ASTResolved -> FuncHeader
getFunctionHeader symbol ast = if Map.member symbol (funcDefs ast) then
        let body = funcDefs ast Map.! symbol in funcHeaderFromBody symbol body
    else if Map.member symbol (funcImports ast) then
        let body = funcImports ast Map.! symbol in funcHeaderFromBody symbol body
    else error "symbol is not function"


getFunctionBody :: Symbol -> ASTResolved -> FuncBody
getFunctionBody symbol ast = if Map.member symbol (funcDefs ast) then
        funcDefs ast Map.! symbol
    else if Map.member symbol (funcImports ast) then
        funcImports ast Map.! symbol
    else error "symbol is not function"


funcHeaderFromBody :: Symbol -> FuncBody -> FuncHeader
funcHeaderFromBody symbol body =
    FuncHeader {
        typeArgs = funcTypeArgs body,
        paramTypes = map typeof (funcParams body),
        symbol = symbol,
        argTypes = map typeof (funcArgs body),
        returnType = funcRetty body
        }


funcHeadersCouldMatch :: ASTResolved -> FuncHeader -> FuncHeader -> Bool
funcHeadersCouldMatch ast a b
    | not $ symbolsCouldMatch (symbol a) (symbol b)                                                          = False
    | length (paramTypes a) /= length (paramTypes b) || length (argTypes a) /= length (argTypes b)           = False
    | not $ all (== True) $ zipWith (typesCouldMatch (typeFuncs ast) generics) (paramTypes a) (paramTypes b) = False
    | not $ all (== True) $ zipWith (typesCouldMatch (typeFuncs ast) generics) (paramTypes a) (paramTypes b) = False
    | not $ typesCouldMatch (typeFuncs ast) generics (returnType a) (returnType b)                           = False
    | otherwise = True
    where
        generics = typeArgs a ++ typeArgs b


prettyFuncBody :: Symbol -> FuncBody -> IO ()
prettyFuncBody symbol body =
    prettyStmt "" $ FuncDef
        undefined
        (funcTypeArgs body)
        (funcParams body)
        symbol
        (funcArgs body)
        (funcRetty body)
        (funcStmt body)


prettyASTResolved :: ASTResolved -> IO ()
prettyASTResolved ast = do
    putStrLn $ "module " ++ moduleName ast

    forM_ (Map.toList $ constDefs ast) $ \(symbol, expr) ->
        prettyStmt "" $ AST.Const undefined symbol expr

    forM_ (Map.toList $ typeFuncs ast) $ \(symbol, (typeArgs, typ)) ->
        prettyStmt "" (AST.Typedef undefined typeArgs symbol $ AnnoType typ)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) -> prettyFuncBody symbol body

    putStrLn ""
