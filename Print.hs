{-# LANGUAGE FlexibleContexts #-}
module Print where

import Control.Monad

import Value
import CompileState
import Monad
import Funcs
import qualified Type as T

valPrint :: InsCmp CompileState m => String -> Value -> m ()
valPrint append val = case valType val of
    T.I64 -> void $ printf ("%d" ++ append) [valOp val]
    T.I32 -> void $ printf ("%d" ++ append) [valOp val]
    _ -> return ()

