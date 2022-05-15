{-# LANGUAGE FlexibleContexts #-}
-- Takes a C AST and generates the necessary definitions
module Interop where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error

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
    = ExtFunc String [Type] Type
    | ExtVar String S.AnnoType
    deriving (Show, Eq)


cTypeToType :: (BoM s m, Show a) => CTypeSpecifier a -> m Type
cTypeToType typeSpec = case typeSpec of
    CIntType _    -> return I64
    CDoubleType _ -> return F64
    CFloatType _  -> return F32
    _             -> fail (show typeSpec)


compile :: BoM [Extern] m => CTranslUnit -> m ()
compile (CTranslUnit cExtDecls _) = forM_ cExtDecls $ \cExtDecl -> case cExtDecl of

    CDeclExt (CDecl [CTypeSpec typeSpec] [(Just (CDeclr (Just (Ident sym _ _)) [] Nothing [] _), Nothing, Nothing)] _) -> do
        typ <- cTypeToType typeSpec
        modify $ \externs -> ExtVar sym (S.AnnoType typ) : externs

    CDeclExt (CDecl [CStorageSpec (CExtern _), CTypeSpec typeSpec] cDeclr _) -> case cDeclr of
        [(Just (CDeclr (Just (Ident sym _ _)) [] Nothing [] _), Nothing, Nothing)] -> do
            typ <- cTypeToType typeSpec
            modify $ \externs -> ExtVar sym (S.AnnoType typ) : externs

        [(Just (CDeclr (Just (Ident sym _ _)) [CFunDeclr (Right ([CDecl [CTypeSpec argTypeSpec] [argDeclr] _], False)) [] _] Nothing attributes _), Nothing, Nothing)] -> do
            let m = do { typ <- cTypeToType typeSpec; argType <- cTypeToType argTypeSpec; modify $ \externs -> ExtFunc sym [argType] typ : externs }
            catchError m $ \e -> return ()

        _ -> return ()

    CDeclExt (CDecl (CStorageSpec (CTypedef _):_) _ _) -> do
        -- do nothing
        return ()

    _ -> return ()
