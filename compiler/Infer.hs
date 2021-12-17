{-# LANGUAGE FlexibleContexts #-}
module Infer where

import Control.Monad.State
import Data.Word
import qualified Data.Map as Map


import AST
import Type
import Error
import Monad

data AType
    = ATypeId Int
    | AType Type
    | AInteger
    | AFloating
    | ANone
    deriving (Show, Eq)

data AExpr
    = AInt        TextPos Integer AType
    | AFloat      TextPos Double AType
    | ABool       TextPos Bool AType
    | AChar       TextPos Char AType
    | ANull       TextPos AType
    | AString     TextPos String AType
    | ATuple      TextPos [AExpr] AType
    | AArray      TextPos [AExpr] AType
    | ATable      TextPos [[AExpr]] AType
    | AMember     TextPos AExpr String AType
    | ASubscript  TextPos AExpr AExpr AType
    | ARange      TextPos AExpr (Maybe AExpr) (Maybe AExpr) AType
    | ATupleIndex TextPos AExpr Word32 AType
    | AIdent      Symbol AType
    | ACall       TextPos AExpr [AExpr] AType
    | AConv       TextPos Type [AExpr] AType
    | ALen        TextPos AExpr AType
    | ACopy       TextPos AExpr AType
    | AAppend     TextPos AExpr AExpr AType
    | APrefix     TextPos Op AExpr AType
    | AInfix      TextPos Op AExpr AExpr AType
    | AAddress    TextPos AExpr AType
    deriving (Eq, Show)


data InferState =
    InferState
    { environment :: Map.Map Int AType
    , intSupply   :: Int
    , variables   :: Map.Map String Int
    }
    deriving (Show)

initInferState =
    InferState
    { environment = Map.empty
    , intSupply   = 0
    , variables   = Map.empty
    }


genNewType :: BoM InferState m => m AType
genNewType = do
    is <- gets intSupply
    env <- gets environment
    modify $ \s -> s { intSupply = is + 1, environment = Map.insert is ANone env }
    return (ATypeId is)


genNewVar :: BoM InferState m => String -> m AType
genNewVar id = do
    ATypeId i <- genNewType
    modify $ \s -> s { variables = Map.insert id i (variables s) }
    return (ATypeId i)


look :: BoM InferState m => String -> m AType
look var = do
    vars <- gets variables
    case Map.lookup var vars of
        Nothing -> fail $ "Cannot find " ++ var
        Just i  -> (Map.! i) <$> gets environment
    
    
    
annotate :: BoM InferState m => Expr -> m AExpr
annotate expr = case expr of
    Int p n      -> AInt  p n <$> genNewType
    Float p f    -> AFloat p f <$> genNewType
    AST.Bool p b -> return $ ABool p b (AType Type.Bool)
    AST.Char p c -> return $ AChar p c (AType Type.Char)
    Null p       -> ANull p <$> genNewType
    String p s   -> return $ AString p s (AType $ Type.Table [Type.Char])
    Ident symbol -> AIdent symbol <$> look (show symbol)

    Infix p op expr1 expr2 -> do
        et1 <- annotate expr1
        et2 <- annotate expr2
        AInfix p op et1 et2 <$> genNewType
        
    _ -> fail $ "cannot annotate: " ++ show expr



