{-# LANGUAGE FlexibleContexts #-}
module Print where

import Control.Monad

import Value
import CompileState
import Monad
import Funcs
import Table
import Type 
import qualified AST as S

import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import qualified LLVM.AST.IntegerPredicate as P


valPrint :: InsCmp CompileState m => String -> Value -> m ()
valPrint append val = case valType val of
    Typedef s -> do
        printf (s ++ "(") []
        base <- valBaseType val
        valPrint ")" (val { valType = base }) 

    I64 -> do
        Val _ op <- valLoad val
        void $ printf ("%d" ++ append) [op]

    Char -> do
        Val _ op <- valLoad val
        void $ printf ("%c" ++ append) [op]

    Bool -> do
        Val _ op <- valLoad val
        str <- globalStringPtr "true\0false" =<< fresh
        idx <- select op (int64 0) (int64 5)
        pst <- gep (cons str) [idx]
        void $ printf ("%s" ++ append) [pst]

    Tuple ts -> do
        printf "(" []
        forM_ (zip ts [0..]) $ \(t, i) -> do
            elem <- valTupleIdx val (fromIntegral i)
            if i < length ts - 1
            then valPrint ", " elem
            else valPrint (")" ++ append) elem

    Table ts -> do
        printf "{" []
        let nrows = fromIntegral (length ts)
        len <- tableLen val
        lenZero <- valsInfix S.LTEq len (valI64 0)

        let m1 = forM_ [0..nrows-1] $ \i -> do
            row <- tableRow i val
            n <- valsInfix S.Minus len (valInt I64 1)
            for (valOp n) $ \j -> valPrint ", " =<< valPtrIdx row (Val I64 j)
            if i < nrows-1
            then valPrint "; " =<< valPtrIdx row n
            else valPrint ("}" ++ append) =<< valPtrIdx row n

        let m2 = void (printf "}" [])
        if_ (valOp lenZero) m2 m1 

    Array n t -> do
        printf "[" []
        for (int64 $ fromIntegral n-1) $ \i -> do
            valPrint ", " =<< valArrayIdx val (Val I64 i)
        valPrint ("]" ++ append) =<< valArrayConstIdx val (n-1)
        
    _ -> error ("print: " ++ show (valType val))
