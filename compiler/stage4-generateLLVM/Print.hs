{-# LANGUAGE FlexibleContexts #-}
module Print where

import Control.Monad

import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import qualified LLVM.AST.Type as LL
import qualified LLVM.AST.IntegerPredicate as P

import qualified AST
import Value
import State
import Monad
import Funcs
import Table
import Tuple
import Type 
import Typeof
import Array
import Builtin
import Error


valPrint :: InsCmp CompileState m => String -> Pointer -> m ()
valPrint append val = withErrorPrefix "valPrint " $ case typeof val of
    t | isInt t   -> void . printf ("%ld" ++ append) . (:[]) . op =<< pload val
    t | isFloat t -> void . printf ("%f" ++ append) . (:[]) . op =<< pload =<< newConvert F64 val
    Char          -> void . printf ("%c" ++ append) . (:[]) . op =<< pload val

    UnsafePtr -> void . printf ("%p" ++ append) . (:[]) . op =<< pload val

    Typedef s     -> do
        base <- baseTypeOf (Typedef s)
        valPrint append =<< toType base val

    Bool -> do
        op <- op <$> pload val
        str <- getStringPointer "true\0false"
        void . printf ("%s" ++ append) . (:[]) =<< gep str . (:[]) =<< select op (int64 0) (int64 5)

    Tuple ts -> do
        printf "(" []
        forM_ (zip ts [0..]) $ \(t, i) -> do
            let app = if i < length ts - 1 then ", " else ""
            valPrint app =<< tupleIdx i val
        void $ printf (")" ++ append) []

    Range t -> do
        printf "[" []
        valPrint ".."            =<< rangeStart val
        valPrint ("]" ++ append) =<< rangeEnd val

    Table [Char] -> do
        (Pointer t p) <- tableRow 0 val
        (Pointer I64 lp) <- tableLen val
        lo <- load lp 0
        void $ printf ("%-.*s" ++ append) [lo, p]

    Table ts -> do
        printf "[" []
        len <- tableLen val
        lenZero <- mkInfix AST.EqEq len =<< newI64 0

        if_ (valOp lenZero)
            (void $ printf ("]" ++ append) [])
            (tablePrintHelper ts (fromPointer val) len)

    Array n t -> do
        printf "[" []
        forM_ [0..n-2] $ \i ->
            valPrint ", " =<< arrayGetElem val =<< pload =<< newI64 i
        valPrint ("]" ++ append) =<< arrayGetElem val =<< pload =<< newI64 (n-1)


    _ -> error ("print: " ++ show (typeof val))

    where
        tablePrintHelper :: InsCmp CompileState m => [Type] -> Value -> Pointer -> m ()
        tablePrintHelper ts val len = forM_ [0..length ts - 1] $ \i -> do
            row@(Pointer t p) <- tableRow i (toPointer val)
            n <- mkInfix AST.Minus len =<< newI64 1

            for (valOp n) $ \j -> valPrint ", " =<< advancePointer row (Value2 I64 j)
            if i < length ts - 1
            then valPrint "; " =<< advancePointer row . toValue =<< valLoad n
            else valPrint ("]" ++ append) =<< advancePointer row . toValue =<< valLoad n
