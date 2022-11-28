{-# LANGUAGE FlexibleContexts #-}
module Print where

import Control.Monad

import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import qualified LLVM.AST.Type as LL
import qualified LLVM.AST.IntegerPredicate as P

import qualified AST as S
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


valPrint :: InsCmp CompileState m => String -> Value -> m ()
valPrint append val = case valType val of
    t | isInt t   -> void . printf ("%ld" ++ append) . (:[]) . valOp =<< valLoad val
    t | isFloat t -> void . printf ("%f" ++ append) . (:[]) . valOp =<< mkConvert F64 val
    Char          -> void . printf ("%c" ++ append) . (:[]) . valOp =<< valLoad val
    Typedef s     -> do
        base <- baseTypeOf (Typedef s)
        case val of
            Val _ op -> valPrint append (Val base op)
            Ptr _ loc -> valPrint append (Ptr base loc)

    Bool -> do
        op <- valOp <$> valLoad val
        str <- globalStringPtr "true\0false" =<< myFreshPrivate "str"
        void . printf ("%s" ++ append) . (:[]) =<< gep (cons str) . (:[]) =<< select op (int64 0) (int64 5)

    Tuple ts -> do
        printf "(" []
        forM_ (zip ts [0..]) $ \(t, i) -> do
            let app = if i < length ts - 1 then ", " else ""
            valPrint app =<< ptrTupleIdx i val
        void $ printf (")" ++ append) []

    String -> do
        Val _ loc <- valLoad val
        void $ printf ("%s" ++ append) [loc]

    Table [Char] -> do
        row <- ptrTableRow 0 val
        len <- mkTableLen val
        void $ printf ("%-.*s" ++ append) [valOp len, valLoc row]

    Table ts -> do
        printf "[" []
        len <- mkTableLen val
        lenZero <- mkInfix S.EqEq len (mkI64 0)

        if_ (valOp lenZero)
            (void $ printf ("]" ++ append) [])
            (tablePrintHelper ts val len)

    Array n t -> do
        printf "[" []
        forM_ [0..n-2] $ \i ->
            valPrint ", " =<< ptrArrayGetElemConst val i
        valPrint ("]" ++ append) =<< ptrArrayGetElemConst val (n-1)

    _ -> error ("print: " ++ show (valType val))

    where
        tablePrintHelper ts val len = forM_ [0..length ts - 1] $ \i -> do
            row <- ptrTableRow i val
            n <- mkInfix S.Minus len (mkI64 1)

            for (valOp n) $ \j -> valPrint ", " =<< ptrIdx row (Val I64 j)
            if i < length ts - 1
            then valPrint "; " =<< ptrIdx row n
            else valPrint ("]" ++ append) =<< ptrIdx row n
