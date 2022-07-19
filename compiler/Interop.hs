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
    CVoidType _   -> return Void
    _             -> fail (show typeSpec)


genExterns :: BoM [Extern] m => CTranslUnit -> m ()
genExterns (CTranslUnit cExtDecls _) = forM_ cExtDecls $ \cExtDecl -> case cExtDecl of
    CDeclExt decl -> case decl of
        CDecl [CTypeSpec typeSpec] [(Just (CDeclr (Just (Ident sym _ _)) [] Nothing [] _), Nothing, Nothing)] _ -> do
            typ <- cTypeToType typeSpec
            modify $ \externs -> ExtVar sym (S.AnnoType typ) : externs

        -- With appended type spec: func return
        CDecl [CStorageSpec (CExtern _), CTypeSpec typeSpec] [(Just something, Nothing, Nothing)]  _ -> case something of

            CDeclr (Just (Ident sym _ _)) cFunDeclrs Nothing attributes _ -> case cFunDeclrs of

                [CFunDeclr (Right ([CDecl [CTypeSpec (CUnsigType _), CTypeSpec (CIntType _)] [argDeclr] _], False)) [] _] -> do
                    let m = do { typ <- cTypeToType typeSpec;  modify $ \externs -> ExtFunc sym [I64] typ : externs }
                    catchError m $ \e -> return ()

                -- C variable
                []  -> do
                    typ <- cTypeToType typeSpec
                    modify $ \externs -> ExtVar sym (S.AnnoType typ) : externs

                -- C function with zero args and return type
                [CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType _)] [] _], False)) [] _] -> do
                    let m = do { typ <- cTypeToType typeSpec;  modify $ \externs -> ExtFunc sym [] typ : externs }
                    catchError m $ \e -> return ()

                -- C function with one arg and return type
                [CFunDeclr (Right ([CDecl [CTypeSpec argTypeSpec] [argDeclr] _], False)) [] _] -> do
                    let m = do { typ <- cTypeToType typeSpec; argType <- cTypeToType argTypeSpec; modify $ \externs -> ExtFunc sym [argType] typ : externs }
                    catchError m $ \e -> return ()

                _ -> return ()

            _ -> return ()

        _ -> return ()

    _ -> return ()


cmpExtern :: InsCmp CompileState m => Extern -> m ()
cmpExtern extern = case extern of
    ExtVar sym (S.AnnoType typ) -> do
        let symbol = SymQualified "c" sym
        let name = mkName sym
        opTyp <- opTypeOf typ
        addSymKeyDec symbol KeyVar name (DecVar opTyp)
        define symbol KeyVar $ ObjVal $ Ptr typ $ cons $ C.GlobalReference (LL.ptr opTyp) name

    ExtFunc sym argTypes retty -> do
        let symbol = SymQualified "c" sym
        checkSymUndef symbol
        let name = LL.mkName sym

        paramOpTypes <- mapM opTypeOf argTypes
        returnOpType <- opTypeOf retty

        addSymKeyDec symbol (KeyFunc argTypes) name (DecExtern paramOpTypes returnOpType False)
        let op = fnOp name paramOpTypes returnOpType False
        define symbol (KeyFunc argTypes) (ObjFunc retty op)


compile :: BoM s m => [Extern] -> m CompileState
compile externs = do
    ((_, defs), state) <- runBoMTExcept (initCompileState [] "c") (runModuleCmpT emptyModuleBuilder cmp)
    return state
    where
        cmp :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        cmp = void $ func (LL.mkName ".__unused")  [] LL.VoidType $ \_ -> do
            mapM_ cmpExtern externs
