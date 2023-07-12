{-# LANGUAGE FlexibleContexts #-}
module CGenerate where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Monad
import Symbol
import States
import CBuilder
import Control.Monad.State
import Type
import AST (Param, paramType, paramName)


cParamOf :: Param -> CParam
cParamOf param = CParam { cName = show (paramName param), cType = cTypeOf (paramType param) }

cTypeOf :: Type -> CType
cTypeOf typ = case typ of
    I64 -> Cint64_t
    I32 -> Cint32_t
    I8 -> Cint8_t
    F64 -> Cdouble
    F32 -> Cfloat
    Bool -> Cbool
    Char -> Cchar
    ADT fs -> Cstruct [CParam "en" Cint64_t, CParam "" $ Cunion $ zipWith (\a b -> CParam ("m" ++ show a) b) [0..] (map (cTypeOf . fieldType) fs)]
    Tuple ts -> Cstruct $ zipWith (\a b -> CParam ("m" ++ show a) b) [0..] (map cTypeOf ts)
    Void -> Cvoid
    Type.Typedef s -> Ctypedef (show s)
    Table ts -> Cstruct (CParam "len" Cint64_t : zipWith (\a b -> CParam ("m" ++ show a) b) [0..] (map (Cpointer . cTypeOf) ts))
    Sparse ts -> Cstruct [CParam "elems" (cTypeOf (Table ts)), CParam "free" (cTypeOf (Table [I64]))]
    _ -> error (show typ)
    where
        fieldType f = case f of
            FieldNull -> I8
            FieldType t -> t
            FieldCtor [t] -> t
            FieldCtor ts -> Tuple ts
        


getSymbols :: Monad m => Type -> m [Symbol]
getSymbols typ = case typ of
    Type.Typedef s -> return [s]
    Tuple ts  -> concat <$> mapM getSymbols ts
    Table ts  -> concat <$> mapM getSymbols ts
    Sparse ts  -> concat <$> mapM getSymbols ts
    I64 -> return []
    I32 -> return []
    F64 -> return []
    F32 -> return []
    ADT fs -> concat <$> mapM getSymbolsField fs
    Bool -> return []
    _ -> error (show typ)
    where
        getSymbolsField :: Monad m => AdtField -> m [Symbol]
        getSymbolsField field = case field of
            FieldNull -> return []
            FieldType t -> getSymbols t
            FieldCtor ts -> concat <$> mapM getSymbols ts
            _ -> error (show field)
        
getSymbolsFromMap :: Monad m => Map.Map Symbol Type -> m [Symbol]
getSymbolsFromMap typedefs = do
    fmap (removeDuplicates . concat) . forM (Map.toList typedefs) $ \(s, t) -> do
        symbols <- getSymbols t
        return (symbols ++ [s])
    where
        removeDuplicates :: Eq a => [a] -> [a]
        removeDuplicates [] = []
        removeDuplicates (x:xs)
            | elem x xs = x:removeDuplicates (deleteAll x xs)
            | otherwise = x:removeDuplicates xs
            where
                deleteAll :: Eq a => a -> [a] -> [a]
                deleteAll a [] = []
                deleteAll a (x:xs)
                    | a == x    = deleteAll a xs
                    | otherwise = x : deleteAll a xs


generate :: BoM CBuilderState m => ResolvedAst -> m ()
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
        let args = map cParamOf (States.funcArgs func)
        let params = map (\(CParam n t) -> CParam n (Cpointer t)) $ map cParamOf (States.funcParams func)
        let rettyType = cTypeOf (States.funcRetty func)

        newFunction rettyType (show symbol) (params ++ args)


        
