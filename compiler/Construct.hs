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
            len <- valLoad =<< tableLen val
            tableSetCap loc len
            tableSetLen loc len

            forM_ (zip ts [0..]) $ \(t, i) -> do
                mal <- valMalloc t len
                tableSetRow loc i mal
                rowVal <- tableRow i val
                rowLoc <- tableRow i loc
                for (valOp len) $ \oi -> do
                    ptr <- valPtrIdx rowLoc (Val I64 oi)
                    valStore ptr =<< valCopy =<< valPtrIdx rowVal (Val I64 oi)
                    
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
valConstruct typ []       = trace "valConstruct" $ zeroOf typ
valConstruct typ (a:b:xs) = trace "valConstruct" $ tupleConstruct typ (a:b:xs)
valConstruct typ [val]    = trace "valConstruct" $ do
    val' <- valLoad =<< valResolveExp val

    if valType val' == typ
    then return val'
    else do
        base <- baseTypeOf typ
        case base of
            Table [Char] -> do
                ObjFunc retty op <- look "string" (KeyFunc [valType val'])
                Val retty <$> call op [(valOp val', [])]

            I32 -> case val' of
                Val I64 op -> Val typ <$> trunc op LL.i32
                Val I8 op  -> Val typ <$> sext op LL.i32

            I64 -> case val' of
                Val Char op -> Val typ <$> sext op LL.i64

            Char -> case val' of
                Val I64 op -> Val typ <$> trunc op LL.i8
                Val I32 op -> Val typ <$> trunc op LL.i8
                _          -> error (show val')

            ADT _       -> adtConstruct typ val'

            _           -> do
                checkTypesCompatible typ (valType val')
                Val typ <$> valOp <$> valLoad val'


valResolveExp :: InsCmp CompileState m => Value -> m Value
valResolveExp val = trace "valResolveExp" $ case val of
    Exp (S.Int p n)   -> valInt I64 n
    Exp (S.Float p f) -> valFloat F64 f
    Exp (S.Null p)    -> zeroOf $ ADT [("", Void)]
    Ptr _ _           -> return val
    Val _ _           -> return val
    _                 -> error ("can't resolve contextual: " ++ show val)
