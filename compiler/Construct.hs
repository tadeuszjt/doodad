{-# LANGUAGE FlexibleContexts #-}
module Construct where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant       

import Type
import State
import Monad
import Tuple
import Value
import ADT
import Typeof
import Trace
import Table
import Funcs
import qualified AST as S



valResolveExp :: InsCmp CompileState m => Value -> m Value
valResolveExp val = trace "valResolveExp" $ case val of
    Exp (S.Int p n)   -> valInt I64 n
    Exp (S.Float p f) -> valFloat F64 f
    Exp (S.Null p)    -> valZero $ ADT [("", Void)]
    Ptr _ _           -> return val
    Val _ _           -> return val
    _                 -> error ("can't resolve contextual: " ++ show val)


valCopy :: InsCmp CompileState m => Value -> m Value
valCopy val = trace "valCopy" $ do
    base <- baseTypeOf (valType val)
    case base of
        t | isSimple t -> valLoad val

        Table ts -> do
            len <- tableLen val
            tab <- tableMake (valType val) len

            forM_ (zip ts [0..]) $ \(t, i) -> do
                baseT <- baseTypeOf t
                valRow <- tableRow i val
                tabRow <- tableRow i tab

                if isSimple baseT
                then valMemCpy tabRow valRow len
                else for (valOp len) $ \op -> do
                    ptr <- valPtrIdx tabRow (Val I64 op)
                    valStore ptr =<< valCopy =<< valPtrIdx valRow (Val I64 op)

            valLoad tab


        Tuple ts -> do
            loc <- valLocal (valType val)
            forM_ (zip ts [0..]) $ \(t, i) ->
                tupleSet loc i =<< valCopy =<< tupleIdx i val
            valLoad loc

        ADT xs -> do
            loc <- valLocal (valType val)
            valStore loc val
            valLoad loc
            -- TODO does this work?
            
        _ -> err $ "Can't handle copy: " ++ show (valType val)


valString :: InsCmp CompileState m => Type -> Value -> m Value
valString typ val = do
    let bufSize = 32
    
    assertBaseType (== Table [Char]) typ
    base <- baseTypeOf (valType val)

    tab <- tableMake typ (valI64 bufSize) 
    row <- tableRow 0 tab
    ptr <- bitcast (valLoc row) (LL.ptr LL.i8)
    op <- valOp <$> valLoad val

    case base of
        I64 -> do
            n <- Val I32 <$> snprintf ptr (int64 bufSize) "%d" [op]
            tableSetLen tab =<< valConstruct I64 [n]
        F64 -> do
            n <- Val I32 <$> snprintf ptr (int64 bufSize) "%f" [op]
            tableSetLen tab =<< valConstruct I64 [n]


    valLoad tab




valConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
valConstruct typ []       = trace "valConstruct" $ valZero typ
valConstruct typ (a:b:xs) = trace "valConstruct" $ tupleConstruct typ (a:b:xs)
valConstruct typ [val']   = trace "valConstruct" $ do
    val <- valLoad =<< valResolveExp val'
    base <- baseTypeOf typ

    case base of
        t | isIntegral t || isFloat t -> convertNumber typ val

        ADT _         -> adtConstruct typ val
        Table [Char] -> do
            res <- lookm (Sym "string") $ KeyFunc [valType val]
            case res of
                Just (ObjFunc retty op) -> Val retty <$> call op [(valOp val, [])]
                Nothing -> valString typ val


        _ -> do
            checkTypesCompatible typ (valType val)
            Val typ <$> valOp <$> valLoad val

    where
        convertNumber :: InsCmp CompileState m => Type -> Value -> m Value
        convertNumber typ (Val valTyp op) = do
            base <- baseTypeOf typ
            baseVal <- baseTypeOf valTyp
            fmap (Val typ) $ case (base, baseVal) of
                (I64,  I64) -> return op
                (I32,  I64) -> trunc op LL.i32
                (I16,  I64) -> trunc op LL.i16
                (I8,   I64) -> trunc op LL.i8
                (Char, I64) -> trunc op LL.i8
                (F64,  I64) -> sitofp op LL.double

                (I64,  I32) -> sext op LL.i64
                (I32,  I32) -> return op
                (I16,  I32) -> trunc op LL.i16
                (I8,   I32) -> trunc op LL.i8
                (Char, I32) -> trunc op LL.i8

                (I64, I16) -> sext op LL.i64
                (I32, I16) -> sext op LL.i32
                (I16, I16) -> return op
                (I8,  I16) -> trunc op LL.i8
                (Char, I16) -> trunc op LL.i8

                (I64 , I8) -> sext op LL.i64
                (I32 , I8) -> sext op LL.i32
                (I16 , I8) -> sext op LL.i16
                (I8  , I8) -> return op
                (Char, I8) -> return op

                (I64,  Char) -> sext op LL.i64
                (I32,  Char) -> sext op LL.i32
                (I16,  Char) -> sext op LL.i16
                (I8,   Char) -> return op
                (Char, Char) -> return op

                (I64, F64) -> fptosi op LL.i64
                x -> err (show x)
