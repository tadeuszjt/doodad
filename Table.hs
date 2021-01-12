{-# LANGUAGE FlexibleContexts #-}
module Table where

import Data.Word


import Monad
import CompileState
import qualified Type as T

import           LLVM.AST.Type              hiding (Type, void, double)
import qualified LLVM.AST.Constant          as C
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad


valTableLen :: InsCmp s m => Value -> m Value
valTableLen (Ptr (T.Table _) loc) = fmap (Ptr T.I64) $ gep loc [int32 0, int32 0]
valTableLen (Val (T.Table _) op)  = fmap (Val T.I64) $ extractValue op [0]


valTableCap :: InsCmp s m => Value -> m Value
valTableCap (Ptr (T.Table _) loc) = fmap (Ptr T.I64) $ gep loc [int32 0, int32 1]
valTableCap (Val (T.Table _) op)  = fmap (Val T.I64) $ extractValue op [1]


valTableRow :: InsCmp s m => Word32 -> Value -> m Value
valTableRow i val = do
    let T.Table ts = valType val
    assert (fromIntegral i < length ts) "table row index >= num rows"
    let t = ts !! fromIntegral i
    case val of
        Val _ op  -> fmap (Ptr t) (extractValue op [i+2])
        Ptr _ loc -> do
            pp <- gep loc [int32 0, int32 $ fromIntegral i+2]
            fmap (Ptr t) (load pp 0)

