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
import Tuple
import Type 
import ADT


valPrint :: InsCmp CompileState m => String -> Value -> m ()
valPrint append val = case valType val of
    t | isInt t   -> void . printf ("%ld" ++ append) . (:[]) . valOp =<< valLoad val
    t | isFloat t -> void . printf ("%f" ++ append) . (:[]) . valOp =<< valLoad val
    Char          -> void . printf ("%c" ++ append) . (:[]) . valOp =<< valLoad val

    Bool -> do
        op <- valOp <$> valLoad val
        str <- globalStringPtr "true\0false" =<< fresh
        void . printf ("%s" ++ append) . (:[]) =<< gep (cons str) . (:[]) =<< select op (int64 0) (int64 5)

    Typedef s -> do
        printf (s ++ "(") []
        base <- baseTypeOf (valType val)
        case val of
            Ptr t loc -> valPrint (")" ++ append) (Ptr base loc)
            Val t op  -> valPrint (")" ++ append) (Val base op)

    ADT [(s, t)] -> do
        void $ printf (show (valType val) ++ "{") []
        void . printf ("%p}" ++ append) . (:[]) . valOp =<< valLoad val
--
--    ADT xs -> do
--        en <- adtEnum val
--
--        let cases = (flip map) (zip ts [0..]) $ \(t, i) ->
--                let b = valOp <$> valsInfix S.EqEq en (valI64 i) in
--                let s = do
--                    case t of
--                        Void      -> printf "null" [] >> return ()
--                        Named n _ -> printf (n ++ "(") [] >> adtConstruct (ADT [t]) val >>= adtDeref >>= valPrint "("
--                        t         -> valPrint "" =<< adtDeref =<< adtConstruct (ADT [t]) val
--                in (b, s)
--
--        switch_ cases >> printf append [] >> return ()

    Tuple ts -> do
        printf "(" []
        forM_ (zip ts [0..]) $ \(t, i) ->
            let app = if i < length ts - 1 then ", " else ""
            in valPrint app =<< tupleIdx i val
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
        for (int64 $ fromIntegral n-1) $ \i ->
            valPrint ", " =<< valArrayIdx val (Val I64 i)
        valPrint ("]" ++ append) =<< valArrayConstIdx val (n-1)

    _ -> error ("print: " ++ show (valType val))
