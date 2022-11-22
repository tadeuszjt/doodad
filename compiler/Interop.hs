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
import Language.C.Syntax.Constants
import qualified Language.C.Analysis as A
import Language.C.Data.Error
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
import Value
import Trace
import Symbol

data Extern
    = ExtFunc String [Type] Type
    | ExtVar String S.AnnoType
    | ExtConstInt String Integer
    | ExtTypeDef String Type
    deriving (Show, Eq)


typeToCType :: Type -> String
typeToCType typ = case typ of
    I64 -> "int"
    F64 -> "double"


macroToVar :: String -> String
macroToVar macro = "c__" ++ macro


importToCStmt :: S.Import -> String
importToCStmt (S.ImportCMacro macro typ) =
    typeToCType typ ++ " " ++ macroToVar macro ++ " = " ++ macro ++ ";"



cmpExtern :: InsCmp CompileState m => Extern -> m ()
cmpExtern extern = catchError (cmpExtern' extern) $ \e -> return ()
    where
        cmpExtern' :: InsCmp CompileState m => Extern -> m ()
        cmpExtern' extern = case extern of
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

                addSymKeyDec symbol (KeyFunc argTypes retty) name (DecExtern paramOpTypes returnOpType False)
                let op = fnOp name paramOpTypes returnOpType False
                define symbol (KeyFunc argTypes retty) (ObjFnOp op)

            ExtConstInt sym integer -> do
                let symbol = SymQualified "c" sym
                define symbol KeyVar (ObjVal (ConstInt integer))

            ExtTypeDef sym typ -> do
                let symbol = SymQualified "c" sym
                define symbol KeyType (ObType typ)


compile :: BoM s m => [Extern] -> m CompileState
compile externs = do
    ((_, defs), state) <- runBoMTExcept (initCompileState [] "c") (runModuleCmpT emptyModuleBuilder cmp)
    return state
    where
        cmp :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        cmp = void $ func (LL.mkName ".__unused")  [] LL.VoidType $ \_ -> do
            mapM_ cmpExtern [ e | e@(ExtTypeDef _ _) <- externs ]
            mapM_ cmpExtern [ e | e@(ExtFunc _ _ _) <- externs ]



analyseCTranslUnit :: BoM s m => CTranslUnit -> m (A.GlobalDecls, [CError])
analyseCTranslUnit ast =do
    case A.runTrav_ $ A.analyseAST ast of
        Left errs -> fail "ast errors"
        Right (globs, errs) -> return (globs, errs)


directTypeToType :: BoM s m => A.TypeName -> m Type
directTypeToType cType = trace "directTypeToType" $ case cType of
    A.TyVoid -> return Void
    A.TyIntegral A.TyUInt -> return I32
    A.TyIntegral A.TyULong -> return I64
    A.TyIntegral A.TyInt -> return I32
    A.TyIntegral A.TyLong -> return I64
    A.TyIntegral A.TyUShort -> return I16
    A.TyIntegral A.TyUChar -> return Char
    A.TyIntegral A.TyChar -> return Char
    A.TyIntegral A.TyShort -> return I16
    A.TyIntegral A.TySChar -> return Char
    A.TyIntegral A.TyLLong -> return I64
    A.TyIntegral A.TyULLong -> return I64
    A.TyFloating A.TyFloat -> return F32
    A.TyFloating A.TyDouble -> return F64
    A.TyFloating (A.TyFloatN _ _) -> fail ""
    A.TyFloating A.TyLDouble -> fail ""
    A.TyBuiltin _ -> fail ""
    A.TyComp _ -> fail ""
    --A.TyEnum a -> error (show a)
    _ -> fail (show cType)
    _ -> error (show cType)

aTypeToType :: BoM s m => A.Type -> m Type
aTypeToType aType = trace "aTypeToType" $ case aType of
    A.TypeDefType (A.TypeDefRef refIdent baseAType _) quals [] -> do
        let Ident refSym _ _ = refIdent
        return $ Typedef (SymQualified "c" refSym)

    A.PtrType aTyp typeQuals [] -> do
        t <- aTypeToType aTyp
        return $ UnsafePtr t

    A.DirectType dType quals _ -> directTypeToType dType

    _ -> fail ""


genExternsFromGlobs :: BoM [Extern] m => A.GlobalDecls -> m ()
genExternsFromGlobs globalDecls = trace "genExterns" $ do
    forM_ (Map.toList $ A.gTypeDefs globalDecls) $ \(ident, typeDef) -> do
        catchError (procTypeDef typeDef) $ \e -> return ()

    forM_ (Map.toList $ A.gObjs globalDecls) $ \(ident, identDecl) -> case identDecl of
        A.FunctionDef (A.FunDef varDecl body _)  -> do
            catchError (procFunVarDecl varDecl) $ \e -> return ()
--
        A.Declaration (A.Decl varDecl nodeInfo) -> do
            catchError (procFunVarDecl varDecl) $ \e -> return ()

        --a -> error (show a)
        _ -> return ()
    where
        procTypeDef :: BoM [Extern] m => A.TypeDef -> m ()
        procTypeDef typeDef = trace "procTypeDef" $ case typeDef of
            A.TypeDef (Ident sym _ _) aType attrs nodeInfo -> do
                typ <- aTypeToType aType
                modify $ \externs -> ExtTypeDef sym typ : externs

            _ -> error (show typeDef)


        procFunVarDecl :: BoM [Extern] m => A.VarDecl -> m ()
        procFunVarDecl (A.VarDecl varName declAttrs varTyp) = trace "procFunVarDecl" $ do
            let A.VarName (Ident sym _ _) Nothing = varName
            case varTyp of
                A.FunctionType (A.FunType funRetty paramDecls False) [] -> do
                    retty <- aTypeToType funRetty
                    assert (length paramDecls < 64) "too many params. infinite?"
                    paramTypes <- mapM procParamDecl paramDecls
                    modify $ \externs -> ExtFunc sym paramTypes retty : externs
                    return ()

                _ -> fail ""

                _ -> error (show varTyp)

        procParamDecl :: BoM [Extern] m => A.ParamDecl -> m Type
        procParamDecl (A.ParamDecl varDecl nodeInfo) = trace "procParamDecl" $ do
            let A.VarDecl varName attrs varTyp = varDecl
            aTypeToType varTyp
        procParamDecl _ = fail ""
    
