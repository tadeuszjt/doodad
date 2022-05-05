{-# LANGUAGE FlexibleContexts #-}
-- Takes a C AST and generates the necessary definitions
module Interop where

import Control.Monad

import State
import Monad
import qualified AST as S
import Type
import Error

import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Pretty
import Language.C.Syntax.AST
import Text.PrettyPrint


cTypeToType :: BoM s m => CTypeSpecifier a -> m Type
cTypeToType typeSpec = case typeSpec of
    CIntType _ -> return I64

compile :: BoM s m => CTranslUnit -> m [S.Stmt]
compile (CTranslUnit cExtDecls _) = forM cExtDecls $ \cExtDecl -> case cExtDecl of
    CDeclExt (CDecl [CTypeSpec typeSpec] [(Just (CDeclr (Just (Ident sym _ _)) [] Nothing [] _), Nothing, Nothing)] _) -> do
        S.ExternVar (TextPos 0 0 0 0) sym sym <$> cTypeToType typeSpec
    _ -> fail (show cExtDecl)
