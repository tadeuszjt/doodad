{-# LANGUAGE FlexibleContexts #-}
module CGenerate where

import qualified Data.Map as Map

import Monad
import Symbol
import States
import CBuilder
import Control.Monad.State
import Type
import AST (paramType)


cTypeOf :: Type -> CType
cTypeOf typ = case typ of
    I64 -> Cint64_t
    I32 -> Cint32_t
    I8 -> Cint8_t
    F64 -> Cdouble
    F32 -> Cfloat
    Bool -> Cbool
    Char -> Cchar
    ADT fs -> Cstruct [Cint64_t, Cunion (map (cTypeOf . fieldType) fs)]
    Tuple ts -> Cstruct (map cTypeOf ts)
    Void -> Cvoid
    Type.Typedef s -> Ctypedef (show s)
    Table ts -> Cstruct (Cint64_t : map (Cpointer . cTypeOf) ts)
    Sparse ts -> Cstruct [cTypeOf (Table ts), cTypeOf (Table [I64])]
    _ -> error (show typ)
    where
        fieldType f = case f of
            FieldNull -> I8
            FieldType t -> t
            FieldCtor [t] -> t
            FieldCtor ts -> Tuple ts


generate :: BoM CBuilderState m => ResolvedAst -> m ()
generate ast = do
    -- generate imported typedefs (should include member types)
    forM_ (Map.toList $ typeImports ast) $ \(symbol, typ) ->
        void $ newTypedef (cTypeOf typ) (show symbol)

    -- generate imported function externs
    forM_ (Map.toList $ funcImports ast) $ \(symbol, funcKey@(pts, s, ats, rt)) -> case symbol of
        SymResolved _ _ _ -> do
            newExtern (show symbol) (cTypeOf rt) (map (Cpointer . cTypeOf) pts ++ map cTypeOf ats)
        _ -> return ()

    -- generate module typedefs
    forM_ (Map.toList $ typeDefs ast) $ \(symbol, typ) ->
        void $ newTypedef (cTypeOf typ) (show symbol)

    forM_ (Map.toList $ funcDefs ast) $ \(symbol, func) -> do
        let argTypes = map (cTypeOf . paramType) (States.funcArgs func)
        let paramTypes = map (Cpointer . cTypeOf . paramType) (States.funcParams func)
        let rettyType = cTypeOf (States.funcRetty func)

        newFunction rettyType (show symbol) (paramTypes ++ argTypes)


        
