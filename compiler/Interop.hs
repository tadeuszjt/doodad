{-# LANGUAGE FlexibleContexts #-}
-- Takes a C AST and generates the necessary definitions
module Interop where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Pretty
import Language.C.Syntax.AST
import Text.PrettyPrint

import LLVM.AST.Name hiding (Func)
import qualified LLVM.AST as LL
import qualified LLVM.AST.Type as LL
import qualified LLVM.AST.Constant as C
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import qualified AST as S
import Monad
import Type
import Error
import State
import Funcs
import Typeof

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


genExterns :: BoM [Extern] m => CTranslUnit -> m ()
genExterns (CTranslUnit cExtDecls _) = forM_ cExtDecls $ \cExtDecl -> case cExtDecl of

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


cmpExtern :: InsCmp CompileState m => Extern -> m ()
cmpExtern extern = case extern of
    ExtVar sym (S.AnnoType typ) -> do
        let name = mkName sym
        opTyp <- opTypeOf typ
        addSymKeyDec sym KeyVar name (DecVar opTyp)
        define sym KeyVar $ ObjVal $ Ptr typ $ cons $ C.GlobalReference (LL.ptr opTyp) name

    ExtFunc sym argTypes retty -> do
        checkSymUndef sym 
        let name = LL.mkName sym

        paramOpTypes <- mapM opTypeOf argTypes
        returnOpType <- opTypeOf retty

        addSymKeyDec sym (KeyFunc argTypes) name (DecExtern paramOpTypes returnOpType False)
        let op = fnOp name paramOpTypes returnOpType False
        define sym (KeyFunc argTypes) (ObjFunc retty op)


compile :: BoM s m => [Extern] -> m CompileState
compile externs = do
    ((_, defs), state) <- runBoMTExcept (initCompileState Map.empty "c") (runModuleCmpT emptyModuleBuilder cmp)
    return state
    where
        cmp :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        cmp = void $ func (LL.mkName ".__unused")  [] LL.VoidType $ \_ -> do
            mapM_ cmpExtern externs
