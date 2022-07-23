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

data Extern
    = ExtFunc String [Type] Type
    | ExtVar String S.AnnoType
    | ExtConstInt String Integer
    | ExtTypeDef String Type
    deriving (Show, Eq)


typeToCType :: Type -> String
typeToCType typ = case typ of
    I64 -> "int"


macroToVar :: String -> String
macroToVar macro = "c__" ++ macro


importToCStmt :: S.Import -> String
importToCStmt (S.ImportCMacro macro typ) =
    typeToCType typ ++ " " ++ macroToVar macro ++ " = " ++ macro ++ ";"


cTypeToType :: (BoM s m, Show a) => CTypeSpecifier a -> m Type
cTypeToType typeSpec = case typeSpec of
    CIntType _                 -> return I64
    CDoubleType _              -> return F64
    CFloatType _               -> return F32
    CVoidType _                -> return Void
    CTypeDef (Ident sym _ _) _ -> return $ Typedef (SymQualified "c" sym)
    _ -> error (show typeSpec)
    _ -> fail (show typeSpec)


declSpecsToType :: BoM s m => [CDeclSpec] -> m Type
declSpecsToType typeSpecs = case typeSpecs of
    [CTypeSpec (CUnsigType _), CTypeSpec (CIntType _)]  -> return I32
    [CTypeQual (CConstQual _), CTypeSpec (CCharType _)] -> return Char
    [CTypeSpec (CIntType _)]                            -> return I32
    [CTypeSpec (CDoubleType _)]                         -> return F64
    [CTypeSpec (CFloatType _)]                          -> return F32
    [CTypeSpec (CTypeDef (Ident sym _ _) _)]            -> return $ Typedef (SymQualified "c" sym)
    _                                                   -> fail (show typeSpecs)


declrIsValid :: BoM s m => CDeclr -> m ()
declrIsValid declr = case declr of
    CDeclr (Just (Ident sym _ _)) []               Nothing [] _ -> return ()
    CDeclr (Just (Ident sym _ _)) [CPtrDeclr [] _] Nothing [] _ -> fail "ptrs not allowed"
    CDeclr Nothing                [CPtrDeclr [] _] Nothing [] _ -> fail "ptrs not allowed"
    --_ -> error (show declr)
    _ -> fail "invalid declr"


defSpecsToType :: BoM s m => [CDeclSpec] -> m Type
defSpecsToType specs = case specs of
    [CStorageSpec (CExtern _), CTypeSpec (CIntType _)]                          -> return I32
    [CStorageSpec (CExtern _), CTypeSpec (CVoidType _)]                         -> return Void
    [CStorageSpec (CExtern _), CTypeSpec (CDoubleType _)]                       -> return F64
    [CTypeSpec (CTypeDef (Ident sym _ _) _)] -> return $ Typedef (SymQualified "c" sym)
    [CTypeSpec (CIntType _)]                                                    -> return I32
    [CTypeSpec (CLongType _), CTypeSpec (CUnsigType _), CTypeSpec (CIntType _)] -> return I64
    [CTypeSpec (CSignedType _), CTypeSpec (CCharType _)]                        -> return I8
    [CTypeSpec (CUnsigType _), CTypeSpec (CCharType _)]                         -> return I8
    [CTypeSpec (CSignedType _), CTypeSpec (CShortType _)]                       -> return I16
    [CTypeSpec (CUnsigType _), CTypeSpec (CShortType _)]                        -> return I16
    [CTypeSpec (CSignedType _), CTypeSpec (CIntType _)]                         -> return I32
    [CTypeSpec (CUnsigType _), CTypeSpec (CIntType _)]                          -> return I32
    [CTypeSpec (CSignedType _), CTypeSpec (CLongType _), CTypeSpec (CLongType _)] -> return I64
    [CTypeSpec (CUnsigType _), CTypeSpec (CLongType _), CTypeSpec (CLongType _)] -> return I64
    [CTypeSpec (CLongType _), CTypeSpec (CIntType _)] -> return I64
    [CTypeSpec (CUnsigType _), CTypeSpec (CLongType _)] -> return I64

    -- struct { cType decl } 
    [CTypeSpec (CSUType (CStruct CStructTag Nothing (Just [CDecl fieldDeclSpecs [(Just (CDeclr (Just (Ident sym _ _)) [] Nothing [] _), Nothing, Nothing)] _]) [] _) _)] -> do
        typ <- declSpecsToType fieldDeclSpecs
        return $ Tuple [typ]

    (CStorageSpec (CExtern _):_) -> fail ""

    [CTypeSpec (CSUType a _)] -> fail ""
    [CTypeSpec (CEnumType a _)] -> fail ""

    --_ -> error (show specs)
    _ -> fail "invalid specs"


argDeclToType :: BoM s m => CDecl -> m Type
argDeclToType decl = case decl of
    CDecl argDeclSpecs [(Just argDeclr, Nothing, Nothing)] _ -> do
        declrIsValid argDeclr
        declSpecsToType argDeclSpecs

    _ -> fail "invalid decl"


genExterns :: BoM [Extern] m => CTranslUnit -> m ()
genExterns (CTranslUnit cExtDecls _) = forM_ cExtDecls $ \cExtDecl -> case cExtDecl of
    CDeclExt decl -> do
        let m = processCDecl decl
        catchError m $ \e -> return ()

    _ -> return ()
    where
        processCDecl cDecl = case cDecl of
            CDecl [CTypeSpec (CIntType _)] [(Just (CDeclr (Just (Ident ('c':'_':'_':sym) _ _)) [] Nothing [] _), Just (CInitExpr (CConst (CIntConst (CInteger i _ _) _)) _), e)] _ -> do
                modify $ \externs -> ExtConstInt sym i : externs

            -- Typedef 
            CDecl (CStorageSpec (CTypedef _) : defSpecs) [(Just cDeclr, Nothing, Nothing)]  _ -> do
                typ <- defSpecsToType defSpecs
                case cDeclr of
                    CDeclr (Just (Ident sym _ _)) [] Nothing [] _ -> do
                        modify $ \externs -> ExtTypeDef sym typ : externs
                        
                    _ -> return ()

            -- Definition
            CDecl defSpecs [(Just cDeclr, Nothing, Nothing)]  _ -> case cDeclr of

                --Variable
                CDeclr (Just (Ident sym _ _)) [] Nothing attributes _ -> do
                    typ <- defSpecsToType defSpecs
                    modify $ \externs -> ExtVar sym (S.AnnoType typ) : externs

                --Function
                CDeclr (Just (Ident sym _ _)) cFunDeclrs Nothing attributes _ -> case cFunDeclrs of
                    -- Zero args (void)
                    [CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType _)] [] _], False)) [] _] -> do
                        retty <- defSpecsToType defSpecs
                        modify $ \externs -> ExtFunc sym [] retty : externs

                    [CFunDeclr (Right (argDecls, False)) [] _] -> do
                        retty <- defSpecsToType defSpecs
                        argTyps <- mapM argDeclToType argDecls
                        modify $ \externs -> ExtFunc sym argTyps retty : externs

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

        addSymKeyDec symbol (KeyFunc argTypes retty) name (DecExtern paramOpTypes returnOpType False)
        let op = fnOp name paramOpTypes returnOpType False
        define symbol (KeyFunc argTypes retty) (ObjFunc op)

    ExtConstInt sym integer -> do
        let symbol = SymQualified "c" sym
        define symbol KeyVar (ObjVal (ConstInt integer))

    ExtTypeDef sym typ -> do
        let symbol = SymQualified "c" sym
        define symbol KeyType (ObType typ Nothing)


compile :: BoM s m => [Extern] -> m CompileState
compile externs = do
    ((_, defs), state) <- runBoMTExcept (initCompileState [] "c") (runModuleCmpT emptyModuleBuilder cmp)
    return state
    where
        cmp :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        cmp = void $ func (LL.mkName ".__unused")  [] LL.VoidType $ \_ -> do
            mapM_ cmpExtern externs
