{-# LANGUAGE FlexibleContexts #-}
module CGenerate where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Monad
import Symbol
import States
import CBuilder as C
import Control.Monad.State
import Type
import AST as S
import Error


cParamOf :: S.Param -> C.Param
cParamOf param = C.Param { C.cName = show (paramName param), C.cType = cTypeOf (paramType param) }

cTypeOf :: Type.Type -> C.Type
cTypeOf typ = case typ of
    I64 -> Cint64_t
    I32 -> Cint32_t
    I8 -> Cint8_t
    F64 -> Cdouble
    F32 -> Cfloat
    Type.Bool -> Cbool
    Type.Char -> Cchar
    Type.ADT fs -> Cstruct [C.Param "en" Cint64_t, C.Param "" $ Cunion $ zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] (map (cTypeOf . fieldType) fs)]
    Type.Tuple ts -> Cstruct $ zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] (map cTypeOf ts)
    Void -> Cvoid
    Type.Typedef s -> Ctypedef (show s)
    Table ts -> Cstruct (C.Param "len" Cint64_t : zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] (map (Cpointer . cTypeOf) ts))
    Sparse ts -> Cstruct [C.Param "elems" (cTypeOf (Table ts)), C.Param "free" (cTypeOf (Table [I64]))]
    _ -> error (show typ)
    where
        fieldType f = case f of
            FieldNull -> I8
            FieldType t -> t
            FieldCtor [t] -> t
            FieldCtor ts -> Type.Tuple ts
        


getSymbolsFromMap :: Monad m => Map.Map Symbol Type.Type -> m [Symbol]
getSymbolsFromMap typedefs = do
    fmap (removeDuplicates . concat) . forM (Map.toList typedefs) $ \(s, t) -> do
        symbols <- getSymbols t
        return (symbols ++ [s])
    where
        getSymbols :: Monad m => Type.Type -> m [Symbol]
        getSymbols typ = case typ of
            Type.Typedef s -> return [s]
            Type.Tuple ts  -> concat <$> mapM getSymbols ts
            Table ts  -> concat <$> mapM getSymbols ts
            Sparse ts  -> concat <$> mapM getSymbols ts
            I64 -> return []
            I32 -> return []
            F64 -> return []
            F32 -> return []
            Type.ADT fs -> concat <$> mapM getSymbolsField fs
            Type.Bool -> return []
            _ -> error (show typ)

        getSymbolsField :: Monad m => AdtField -> m [Symbol]
        getSymbolsField field = case field of
            FieldNull -> return []
            FieldType t -> getSymbols t
            FieldCtor ts -> concat <$> mapM getSymbols ts
            _ -> error (show field)
        
        removeDuplicates :: Eq a => [a] -> [a]
        removeDuplicates [] = []
        removeDuplicates (x:xs)
            | elem x xs = x:removeDuplicates (deleteAll x xs)
            | otherwise = x:removeDuplicates xs

        deleteAll :: Eq a => a -> [a] -> [a]
        deleteAll a [] = []
        deleteAll a (x:xs)
            | a == x    = deleteAll a xs
            | otherwise = x : deleteAll a xs


generate :: BoM C.BuilderState m => ResolvedAst -> m ()
generate ast = do
    -- problem is, we have a bunch of typedefs : (Symbol, Type)
    -- we want to define them in an order which prevents unknown symbols
    let typedefs = Map.union (typeImports ast) (typeDefs ast)
    orderedSymbols <- getSymbolsFromMap typedefs
    forM_ orderedSymbols $ \symbol -> do
        when (Map.member symbol typedefs) $ do
            void $ newTypedef (cTypeOf $ typedefs Map.! symbol) (show symbol)
            
    -- generate imported function externs
    forM_ (Map.toList $ funcImports ast) $ \(symbol, funcKey@(pts, s, ats, rt)) -> case symbol of
        SymResolved _ _ _ -> do
            newExtern (show symbol) (cTypeOf rt) (map (Cpointer . cTypeOf) pts ++ map cTypeOf ats)
        _ -> return ()

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
        generateFunc symbol func


generateFunc :: BoM C.BuilderState m => Symbol -> FuncBody -> m ()
generateFunc symbol body = do
    let args = map cParamOf (States.funcArgs body)
    let params = map (\(C.Param n t) -> C.Param n (Cpointer t)) $ map cParamOf (States.funcParams body)
    let rettyType = cTypeOf (States.funcRetty body)
    setCurrentId =<< newFunction rettyType (show symbol) (params ++ args)
    mapM_ generateStmt (States.funcStmts body)
        


generateStmt :: BoM C.BuilderState m => S.Stmt -> m ()
generateStmt stmt = withPos stmt $ case stmt of
    S.Return _ (Just expr) -> do
        --append =<< newElement . C.Return =<< generateExpr expr
        return ()

    _ -> return ()


generateExpr :: BoM C.BuilderState m => Expr -> m C.Expression
generateExpr (AExpr typ expr) = withPos expr $ case expr of
    S.Bool _ b -> return $ C.Bool b
    S.Infix _ op a b -> do
        ca <- generateExpr a
        cb <- generateExpr b
        cop <- case op of
            S.OrOr -> return C.OrOr
        return $ C.Infix cop ca cb

    _ -> error (show expr)
