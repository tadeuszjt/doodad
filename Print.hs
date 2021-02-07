{-# LANGUAGE FlexibleContexts #-}
module Print where

import Control.Monad

import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import qualified LLVM.AST.Type as LL

import qualified AST as S
import Value
import CompileState
import Monad
import Funcs
import Table
import Type 
import ADT


valPrint :: InsCmp CompileState m => String -> Value -> m ()
valPrint append val = case unNamed (valType val) of
    Typedef s -> do
        printf (s ++ "(") []
        base <- baseTypeOf (valType val)
        case val of
            Ptr t loc -> valPrint (")" ++ append) (Ptr base loc)
            Val t op  -> valPrint (")" ++ append) (Val base op)

    ADT [t] -> do
        void $ printf (show (valType val) ++ "{") []
        op <- fmap valOp (valLoad val)
        void $ printf ("%p}" ++ append) [op]

    ADT ts -> do
        en <- adtEnum val

        cases <- forM (zip ts [0..]) $ \(t, i) -> do
            let b = fmap valOp $ valsInfix S.EqEq en (valI64 i)
            let s = do
                if t /= Void then do
                    case t of
                        Named n t -> do
                            void $ printf (n ++ "(") []
                            ptr <- adtConstruct (ADT [Named n t]) val
                            loc <- adtDeref ptr
                            valPrint ")" loc
                        t -> do
                            ptr <- adtConstruct (ADT [t]) val
                            loc <- adtDeref ptr
                            valPrint "" loc
                else do
                    void $ printf "null" []
            return (b, s)

        switch_ cases

        void $ printf append []


    t | isInt t -> do
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
            elem <- valTupleIdx val i
            if i < length ts - 1
            then valPrint ", " elem
            else valPrint "" elem
        void $ printf (")" ++ append) []

    Table [Char] -> do
        row <- tableRow 0 val
        len <- tableLen val
        void $ printf ("%-.*s" ++ append) [valOp len, valLoc row]

    Table ts -> do
        printf "[" []
        let nrows = (length ts)
        len <- tableLen val
        lenZero <- valsInfix S.LTEq len (valI64 0)

        let m1 = forM_ [0..length ts - 1] $ \i -> do
            row <- tableRow i val
            n <- valsInfix S.Minus len (valInt I64 1)
            for (valOp n) $ \j -> valPrint ", " =<< valPtrIdx row (Val I64 j)
            if i < length ts - 1
            then valPrint "; " =<< valPtrIdx row n
            else valPrint ("]" ++ append) =<< valPtrIdx row n

        let m2 = void $ printf ("]" ++ append) []
        if_ (valOp lenZero) m2 m1 

    Array n t -> do
        printf "[%d| " $ (:[]) $ valOp (valI64 n)
        for (int64 $ fromIntegral n-1) $ \i -> do
            valPrint ", " =<< valArrayIdx val (Val I64 i)
        valPrint ("]" ++ append) =<< valArrayConstIdx val (n-1)

    _ -> error ("print: " ++ show (valType val))
