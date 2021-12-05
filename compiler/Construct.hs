{-# LANGUAGE FlexibleContexts #-}
module Construct where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Instruction       

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


valCopy :: InsCmp CompileState m => Value -> m Value
valCopy val = trace "valCopy" $ do
    base <- baseTypeOf (valType val)
    case base of
        t | isSimple t -> valLoad val

        Table ts -> do
            loc <- valLocal (valType val) 
            len <- tableLen val
            tableSetCap loc len
            tableSetLen loc len

            forM_ (zip ts [0..]) $ \(t, i) -> do
                mal <- valMalloc t len
                tableSetRow loc i mal
                row <- tableRow i val
                base <- baseTypeOf t

                if isSimple base
                then valMemCpy mal row len
                else for (valOp len) $ \op -> do
                    ptr <- valPtrIdx mal (Val I64 op)
                    valStore ptr =<< valCopy =<< valPtrIdx row (Val I64 op)
                    
            valLoad loc

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


valConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
valConstruct typ []       = trace "valConstruct" $ valZero typ
valConstruct typ (a:b:xs) = trace "valConstruct" $ tupleConstruct typ (a:b:xs)
valConstruct typ [val']   = trace "valConstruct" $ do
    val <- valLoad =<< valResolveExp val'

    base <- baseTypeOf typ
    baseVal <- baseTypeOf (valType val)

    case base of
        t | isIntegral t -> convertIntegral baseVal typ val
        ADT _            -> adtConstruct typ val
        Table [Char] -> do
            ObjFunc retty op <- look (Sym "string") $ KeyFunc [valType val]
            Val retty <$> call op [(valOp val, [])]

        _ -> do
            checkTypesCompatible typ (valType val)
            Val typ <$> valOp <$> valLoad val

    where
        convertIntegral :: InsCmp CompileState m => Type -> Type -> Value -> m Value
        convertIntegral fromTyp toTyp val = case (fromTyp, toTyp) of
            (I64, I64)  -> return $ Val toTyp (valOp val)
            (I64, I32)  -> Val toTyp <$> trunc (valOp val) LL.i32
            (I64, I16)  -> Val toTyp <$> trunc (valOp val) LL.i16
            (I64, I8)   -> Val toTyp <$> trunc (valOp val) LL.i8
            (I64, Char) -> Val toTyp <$> trunc (valOp val) LL.i8

            (I32, I64) -> Val toTyp <$> sext (valOp val) LL.i64
            (I32, I32) -> return $ Val toTyp (valOp val)
            (I32, I16) -> Val toTyp <$> trunc (valOp val) LL.i16
            (I32, I8)  -> Val toTyp <$> trunc (valOp val) LL.i8
            (I32, Char) -> Val toTyp <$> trunc (valOp val) LL.i8

            (I16, I64) -> Val toTyp <$> sext (valOp val) LL.i64
            (I16, I32) -> Val toTyp <$> sext (valOp val) LL.i32
            (I16, I16) -> return $ Val toTyp (valOp val)
            (I16, I8)  -> Val toTyp <$> trunc (valOp val) LL.i8
            (I16, Char) -> Val toTyp <$> trunc (valOp val) LL.i8

            (I8, I64) -> Val toTyp <$> sext (valOp val) LL.i64
            (I8, I32) -> Val toTyp <$> sext (valOp val) LL.i32
            (I8, I16) -> Val toTyp <$> sext (valOp val) LL.i16
            (I8, I8)  -> return $ Val toTyp (valOp val)
            (I8, Char) -> return $ Val toTyp (valOp val)

            (Char, I64) -> Val toTyp <$> sext (valOp val) LL.i64
            (Char, I32) -> Val toTyp <$> sext (valOp val) LL.i32
            (Char, I16) -> Val toTyp <$> sext (valOp val) LL.i16
            (Char, I8)  -> return $ Val toTyp (valOp val)
            (Char, Char) -> return $ Val toTyp (valOp val)


valResolveExp :: InsCmp CompileState m => Value -> m Value
valResolveExp val = trace "valResolveExp" $ case val of
    Exp (S.Int p n)   -> valInt I64 n
    Exp (S.Float p f) -> valFloat F64 f
    Exp (S.Null p)    -> valZero $ ADT [("", Void)]
    Ptr _ _           -> return val
    Val _ _           -> return val
    _                 -> error ("can't resolve contextual: " ++ show val)
