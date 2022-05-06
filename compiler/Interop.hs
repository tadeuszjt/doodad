{-# LANGUAGE FlexibleContexts #-}
-- Takes a C AST and generates the necessary definitions
module Interop where

import Control.Monad
import Control.Monad.State

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


data Extern
    = ExtFunc String [(String, Type)] Type
    | ExtVar String S.AnnoType
    deriving (Show, Eq)


cTypeToType :: BoM s m => CTypeSpecifier a -> m Type
cTypeToType typeSpec = case typeSpec of
    CIntType _ -> return I64

compile :: BoM [Extern] m => CTranslUnit -> m ()
compile (CTranslUnit cExtDecls _) = forM_ cExtDecls $ \cExtDecl -> case cExtDecl of
    CDeclExt (CDecl [CTypeSpec typeSpec] [(Just (CDeclr (Just (Ident sym _ _)) [] Nothing [] _), Nothing, Nothing)] _) -> do
        typ <- cTypeToType typeSpec
        modify $ \externs -> ExtVar sym (S.AnnoType typ) : externs
    _ -> fail (show cExtDecl)
