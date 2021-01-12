{-# LANGUAGE FlexibleContexts #-}
module Print where

import Control.Monad

import Value
import CompileState
import Monad
import Funcs
import Table
import qualified Type as T

import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import qualified LLVM.AST.IntegerPredicate as P


valPrint :: InsCmp CompileState m => String -> Value -> m ()
valPrint append val = case valType val of
    T.I64 -> do
        Val _ op <- valLoad val
        void $ printf ("%d" ++ append) [valOp val]

    T.Char -> do
        Val _ op <- valLoad val
        void $ printf ("%c" ++ append) [op]

    T.Table ts -> do
        printf "{" []
        Val _ op <- valLoad val
        let nrows = fromIntegral (length ts)
        Val T.I64 len <- valLoad =<< valTableLen val

        lenZero <- icmp P.SLE len (int64 0)

        let m1 = forM_ [0..nrows-1] $ \i -> do
            row <- valTableRow i val

            n <- sub len (int64 1)
            for n $ \j ->
                valPrint ", " =<< valPtrIdx row (Val T.I64 j)
            let app = if i < nrows-1 then "; " else "}" ++ append
            valPrint app =<< valPtrIdx row (Val T.I64 n)

        let m2 = void (printf "}" [])

        if_ lenZero m2 m1 

    T.Array n t -> do
        printf "[" []
        for (int64 $ fromIntegral n-1) $ \i -> do
            valPrint ", " =<< valArrayIdx val (Val T.I64 i)
        valPrint ("]" ++ append) =<< valArrayConstIdx val (n-1)
        
    _ -> error ("print: " ++ show (valType val))
