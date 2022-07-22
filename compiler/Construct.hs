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

        _ -> fail $ "Can't handle copy: " ++ show (valType val)


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
        _ -> fail $ "No string function for: " ++ show (valType val)
        


    valLoad tab




valConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
valConstruct typ []       = trace "valConstruct" $ valZero typ
valConstruct typ (a:b:xs) = trace "valConstruct" $ tupleConstruct typ (a:b:xs)
valConstruct typ [val']   = trace "valConstruct" $ do
    val <- valLoad val'
    base <- baseTypeOf typ

    case base of
        t | isIntegral t || isFloat t -> valConvertNumber typ val

        Table [Char] -> do
            res <- lookm (Sym "string") $ KeyFunc [valType val] typ
            case res of
                Just (ObjFunc op) -> Val typ <$> call op [(valOp val, [])]
                Nothing -> valString typ val


        _ -> do
            checkTypesCompatible typ (valType val)
            Val typ . valOp <$> valLoad val
