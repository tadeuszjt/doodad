{-# LANGUAGE FlexibleContexts #-}
-- Takes a C AST and generates the necessary definitions
module Interop where

import State
import Monad

import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Pretty
import Language.C.Syntax.AST
import Text.PrettyPrint

compile :: InsCmp CompileState m => CTranslUnit -> m ()
compile cTranslUnit = do
    return ()
