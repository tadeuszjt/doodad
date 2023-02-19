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


valPrint :: InsCmp CompileState m => String -> Value -> m ()
valPrint append val = withErrorPrefix "valPrint " $ case typeof val of
    t | isInt t   -> void . printf ("%ld" ++ append) . (:[]) . valOp =<< valLoad val
    t | isFloat t -> void . printf ("%f" ++ append) . (:[]) . op =<< pload =<< newConvert F64 val
    Char          -> void . printf ("%c" ++ append) . (:[]) . valOp =<< valLoad val

    UnsafePtr -> void . printf ("%p" ++ append) . (:[]) . valOp =<< valLoad val

    Typedef s     -> do
        base <- baseTypeOf (Typedef s)
        case val of
            Val _ op -> valPrint append (Val base op)
            Ptr _ loc -> valPrint append (Ptr base loc)

    Bool -> do
        op <- valOp <$> valLoad val
        str <- getStringPointer "true\0false"
        void . printf ("%s" ++ append) . (:[]) =<< gep str . (:[]) =<< select op (int64 0) (int64 5)

    Tuple ts -> do
        printf "(" []
        forM_ (zip ts [0..]) $ \(t, i) -> do
            let app = if i < length ts - 1 then ", " else ""
            valPrint app . fromValue =<< valTupleIdx i . toValue =<< valLoad val
        void $ printf (")" ++ append) []

    Range t -> do
        printf "[" []
        valPrint ".."            . fromPointer =<< rangeStart (toPointer val)
        valPrint ("]" ++ append) . fromPointer =<< rangeEnd (toPointer val)

    Table [Char] -> do
        (Pointer t p) <- tableRow 0 (toPointer val)
        (Pointer I64 lp) <- tableLen (Pointer (typeof val) (valLoc val))
        lo <- load lp 0
        void $ printf ("%-.*s" ++ append) [lo, p]

    Table ts -> do
        printf "[" []
        len <- tableLen (toPointer val)
        lenZero <- mkInfix AST.EqEq len =<< newI64 0

        if_ (valOp lenZero)
            (void $ printf ("]" ++ append) [])
            (tablePrintHelper ts val len)

    Array n t -> do
        printf "[" []
        forM_ [0..n-2] $ \i ->
            valPrint ", " =<< ptrArrayGetElemConst val i
        valPrint ("]" ++ append) =<< ptrArrayGetElemConst val (n-1)


    _ -> error ("print: " ++ show (typeof val))

    where
        tablePrintHelper :: InsCmp CompileState m => [Type] -> Value -> Pointer -> m ()
        tablePrintHelper ts val len = forM_ [0..length ts - 1] $ \i -> do
            row@(Pointer t p) <- tableRow i (toPointer val)
            n <- mkInfix AST.Minus len =<< newI64 1

            for (valOp n) $ \j -> valPrint ", " =<< fromPointer <$> advancePointer row (Value2 I64 j)
            if i < length ts - 1
            then valPrint "; " =<< fromPointer <$> (advancePointer row . toValue =<< valLoad n)
            else valPrint ("]" ++ append) =<< fromPointer <$> (advancePointer row . toValue =<< valLoad n)
