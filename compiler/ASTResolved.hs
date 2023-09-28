module ASTResolved where

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
        , typeFuncs       :: Map.Map Symbol ([Symbol], Type) -- defined type functions
        , ctorDefs        :: Map.Map Symbol (Symbol, Int)    -- defined ctors
        , funcImports     :: Map.Map Symbol FuncBody         -- imported funcs
        , funcDefs        :: Map.Map Symbol FuncBody         -- defined functions
        , symSupply       :: Map.Map String Int              -- type supply from resovle
        }
    deriving (Eq)


type FuncKey = ([Type], Symbol, [Type], Type) -- used to find functions
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


isGenericBody :: FuncBody -> Bool
isGenericBody (FuncBody [] _ _ _ _) = False
isGenericBody _                     = True


funcKeyFromBody :: Symbol -> FuncBody -> FuncKey
funcKeyFromBody symbol body =
    (map typeof (funcParams body), symbol, map typeof (funcArgs body), funcRetty body)


funcKeysCouldMatch :: FuncKey -> FuncKey -> Bool
funcKeysCouldMatch (aParamTypes, aSymbol, aArgTypes, aRetty) (bParamTypes, bSymbol, bArgTypes, bRetty)
    | length aParamTypes /= length bParamTypes || length aArgTypes /= length bArgTypes = False
    | not $ symbolsCouldMatch aSymbol bSymbol                                          = False
    | not $ all (== True) $ zipWith typesCouldMatch aParamTypes bParamTypes            = False
    | not $ all (== True) $ zipWith typesCouldMatch aArgTypes bArgTypes                = False
    | not $ typesCouldMatch aRetty bRetty                                              = False
    | otherwise = True


prettyASTResolved :: ASTResolved -> IO ()
prettyASTResolved ast = do
    putStrLn $ "module: " ++ moduleName ast

    forM_ (Map.toList $ constDefs ast) $ \(symbol, expr) ->
        prettyStmt "" $ AST.Const undefined symbol expr

    forM_ (Map.toList $ typeFuncs ast) $ \(symbol, (args, typ)) -> 
        prettyStmt "" (AST.Typedef undefined (map Symbol.sym args) symbol $ AnnoType typ)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, body) -> 
        prettyStmt "" $ FuncDef
            undefined
            (funcTypeArgs body)
            (funcParams body)
            symbol
            (funcArgs body)
            (funcRetty body)
            (funcStmt body)

    putStrLn ""
