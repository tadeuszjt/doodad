{-# LANGUAGE FlexibleContexts #-}
-- Takes a C AST and generates the necessary definitions
module Interop where

import Data.Maybe
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
import States

typeToCType :: Type -> String
typeToCType typ = case typ of
    I64 -> "int"
    F64 -> "double"


macroToVar :: String -> String
macroToVar macro = "c__" ++ macro


importToCStmt :: S.Import -> String
importToCStmt (S.ImportCMacro macro typ) =
    typeToCType typ ++ " " ++ macroToVar macro ++ " = " ++ macro ++ ";"


astGenExtern :: BoM ResolvedAst m => Extern -> m ()
astGenExtern extern = case extern of
    ExtFunc sym argTypes retty -> do
        let symbol = SymQualified "c" sym
        let key    = ([], sym, argTypes, retty)
        resm <- Map.lookup symbol <$> gets funcDefs
        when (isJust resm) $ do
            fail "c symbol already defined, todo -- allow redef?"

        let body = FuncBody { 
            funcParams = [],
            funcArgs   = map (\t -> S.Param undefined undefined t) argTypes,
            funcRetty  = retty,
            funcStmts  = []
        }

        modify $ \s -> s { funcDefs = Map.insert symbol body (funcDefs s) }
                
    _ -> return () 


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
    A.TyFloating (A.TyFloatN _ _) -> fail (show cType)
    A.TyFloating A.TyLDouble -> fail (show cType)
    A.TyBuiltin _ -> fail (show cType)
    A.TyComp _ -> fail (show cType)
    --A.TyEnum a -> error (show a)
    _ -> fail (show cType)
    _ -> error (show cType)


aTypeToType :: BoM s m => A.Type -> m Type
aTypeToType aType = trace "aTypeToType" $ case aType of
    A.TypeDefType (A.TypeDefRef refIdent baseAType _) quals [] -> do
        let Ident refSym _ _ = refIdent
        return $ Typedef (SymQualified "c" refSym)

    A.PtrType aTyp typeQuals [] -> do
        --t <- aTypeToType aTyp
        return $ UnsafePtr

    A.DirectType dType quals _ -> do
        catchError (directTypeToType dType) $ \e -> fail (show quals)

    _ -> fail (show aType)


genExternsFromGlobs :: BoM [Extern] m => A.GlobalDecls -> m ()
genExternsFromGlobs globalDecls = trace "genExterns" $ do
    forM_ (Map.toList $ A.gTypeDefs globalDecls) $ \(ident, typeDef) -> do
        catchError (procTypeDef typeDef) $ \e -> return ()

    forM_ (Map.toList $ A.gObjs globalDecls) $ \(ident, identDecl) -> case identDecl of
        A.FunctionDef (A.FunDef varDecl body _)  -> do
            catchError (procFunVarDecl varDecl) $ \e -> fail (show varDecl)
--
        A.Declaration (A.Decl varDecl nodeInfo) -> do
            catchError (procFunVarDecl varDecl) $ \e -> return ()

        --a -> error (show a)
        _ -> return ()
    where
        procTypeDef :: BoM [Extern] m => A.TypeDef -> m ()
        procTypeDef typeDef = withErrorPrefix "procTypeDef " $ case typeDef of
            A.TypeDef _ (A.DirectType (A.TyBuiltin A.TyVaList) _ _) _ _ -> return ()

            A.TypeDef (Ident sym _ _) aType attrs nodeInfo -> do
                typ <- aTypeToType aType
                modify $ \externs -> ExtTypeDef sym typ : externs

            _ -> error (show typeDef)


        procFunVarDecl :: BoM [Extern] m => A.VarDecl -> m ()
        procFunVarDecl (A.VarDecl varName declAttrs varTyp) = trace "procFunVarDecl" $ do
            let A.VarName (Ident sym _ _) _ = varName
            case varTyp of
                A.FunctionType (A.FunType funRetty paramDecls False) [] -> do
                    retty <- aTypeToType funRetty
                    assert (length paramDecls < 64) "too many params. infinite?"
                    paramTypes <- mapM procParamDecl paramDecls
                    modify $ \externs -> ExtFunc sym paramTypes retty : externs
                    return ()

                _ -> fail (show varTyp)

                _ -> error (show varTyp)

        procParamDecl :: BoM [Extern] m => A.ParamDecl -> m Type
        procParamDecl (A.ParamDecl varDecl nodeInfo) = trace "procParamDecl" $ do
            let A.VarDecl varName attrs varTyp = varDecl
            aTypeToType varTyp
        procParamDecl x = fail (show x)
    
