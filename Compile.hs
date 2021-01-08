{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Short      as BSS

import           Data.Maybe
import           Control.Monad.Except       hiding (void)
import           Control.Monad.State        hiding (void)
import           Control.Monad.Trans
import           Control.Monad.Fail         hiding (fail)
import           Control.Monad.Identity     
import qualified Data.Set as Set
import qualified Data.Map as Map
import           LLVM.AST                   hiding (function)
import           LLVM.AST.Global
import           LLVM.AST.Constant          as C
import           LLVM.AST.Type              hiding (void)
import qualified LLVM.AST.Constant          as C
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import Monad
import Error
import qualified AST as S
import qualified Type as T
import qualified SymTab
import qualified Flatten as F

mkBSS = BSS.toShort . BS.pack

data SymKey
    = KeyType
    | KeyVar
    | KeyFunc [T.Type]
    deriving (Show, Eq, Ord)


data Object
    = ObType    T.Type   (Maybe Name)
    | ObjExtern [T.Type] (Maybe T.Type) Operand
    | ObjFunc   (Maybe T.Type) Operand
    | ObjVal
    deriving (Show)


data Declaration
    = DecType   Name 
    | DecExtern Name [Type] Type Bool
    | DecFunc   Name [Type] Type
    deriving (Show)


data CompileState
    = CompileState
        { imports      :: Map.Map S.ModuleName CompileState
        , declarations :: Map.Map (S.Symbol, SymKey) Declaration
        , declared     :: Set.Set (S.Symbol, SymKey)
        , definitions  :: [Definition]
        , symTab       :: SymTab.SymTab S.Symbol SymKey Object
        }
    deriving (Show)

initCompileState
     = CompileState
        { imports      = Map.empty
        , declarations = Map.empty
        , declared     = Set.empty
        , definitions  = []
        , symTab       = SymTab.initSymTab
        }



addObj :: BoM CompileState m => S.Symbol -> SymKey -> Object -> m ()
addObj sym key obj =
    modify $ \s -> s { symTab = SymTab.insert sym key obj (symTab s) }


look :: ModCmp CompileState m => S.Symbol -> SymKey -> m Object
look sym key = do
    ensureDeclared sym key
    res <- fmap (SymTab.lookupSymKey sym key) (gets symTab)
    case res of
        Just obj -> return obj
        Nothing  -> do
            r <- fmap (catMaybes . map (SymTab.lookupSymKey sym key . symTab) . Map.elems) (gets imports)
            case r of
                [] -> error ("no obj for: " ++ sym)
                [x] -> return x


checkSymKeyUndef :: BoM CompileState m => S.Symbol -> SymKey -> m ()
checkSymKeyUndef sym key = do
    res <- fmap (SymTab.lookupSymKey sym key) (gets symTab)
    when (isJust res) $ fail (sym ++ " already defined")


checkSymUndef :: BoM CompileState m => S.Symbol -> m ()
checkSymUndef sym = do
    res <- fmap (SymTab.lookupSym sym) (gets symTab)
    when (isJust res) $ fail (sym ++ " already defined")


addDeclared :: BoM CompileState m => S.Symbol -> SymKey -> m ()
addDeclared sym key =
    modify $ \s -> s { declared = Set.insert (sym, key) (declared s) }


addDeclaration :: BoM CompileState m => S.Symbol -> SymKey -> Declaration -> m ()
addDeclaration sym key dec =
    modify $ \s -> s { declarations = Map.insert (sym, key) dec (declarations s) }


ensureDeclared :: ModCmp CompileState m => S.Symbol -> SymKey -> m ()
ensureDeclared sym key = do
    isDeclared <- fmap (Set.member (sym, key)) (gets declared)

    when (not isDeclared) $ do
        res <- fmap (Map.lookup (sym, key)) (gets declarations)
        ress <- fmap (map (Map.lookup (sym, key) . declarations) . Map.elems) (gets imports)

        case catMaybes (res:ress) of
            []  -> return ()
            [r] -> case r of
                DecType nm                               -> void (typedef nm Nothing)
                DecExtern name paramTypes retType isVarg -> do
                    emitDefn $ GlobalDefinition $ functionDefaults
                        { returnType = retType
                        , name       = name
                        , parameters = ([Parameter typ (mkName "") [] | typ <- paramTypes], isVarg)
                        }

    modify $ \s -> s { declared = Set.insert (sym, key) (declared s) }

pushSymTab :: BoM CompileState m => m ()
pushSymTab =
    modify $ \s -> s { symTab = SymTab.push (symTab s) }

popSymTab :: BoM CompileState m => m ()
popSymTab =
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


compileFlatState
    :: (Monad m, MonadFail m, MonadIO m)
    => Map.Map S.ModuleName CompileState
    -> F.FlattenState
    -> m (Either CmpError CompileState)
compileFlatState importCompiled flatState = do
    res <- runModuleCmpT emptyModuleBuilder (initCompileState { imports = importCompiled }) f
    case res of
        Left err                  -> return (Left err)
        Right (((), defs), state) -> return (Right state { definitions = defs })
    where
            f :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
            f = void $ function "main" [] VoidType $ \_ ->
                    getInstrCmp cmp

            cmp :: InsCmp CompileState m => m ()
            cmp = do
                forM_ (Map.toList $ F.typeDefs flatState) $ \(flat, (pos, typ)) -> cmpTypeDef flat pos typ
                mapM_ cmpExternDef (F.externDefs flatState)
                mapM_ cmpFuncDef (F.funcDefs flatState)


cmpTypeDef :: InsCmp CompileState m => S.Symbol-> TextPos -> T.Type -> m ()
cmpTypeDef sym pos typ = do
    checkSymKeyUndef sym KeyType
    case typ of
        T.I8        -> addObj sym KeyType (ObType typ Nothing)
        T.I64       -> addObj sym KeyType (ObType typ Nothing)
        T.Bool      -> addObj sym KeyType (ObType typ Nothing)
        T.Typedef f -> addObj sym KeyType (ObType typ Nothing)
        T.Tuple ts  -> do
            name <- freshName (mkBSS sym)
            opTyp <- opTypeOf typ
            typedef name (Just opTyp)
            addDeclared sym KeyType
            addDeclaration sym KeyType (DecType name)
            addObj sym KeyType $ ObType typ (Just name)
        _ -> error (show typ)
    return ()


cmpExternDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpExternDef (S.Extern pos sym params mretty) = do
    checkSymUndef sym 
    let name = mkName sym
    let paramTypes = map S.paramType params

    pushSymTab
    paramOpTypes <- forM params $ \(S.Param p s t) -> do
        checkSymKeyUndef s KeyVar
        opTypeOf t
    returnOpType <- maybe (return VoidType) opTypeOf mretty
    popSymTab

    addDeclaration sym (KeyFunc paramTypes) (DecExtern name paramOpTypes returnOpType False)


cmpFuncDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpFuncDef (S.Func pos sym params mretty blk) = do
    let paramTypes = map S.paramType params
    checkSymKeyUndef sym (KeyFunc paramTypes)
    name <- freshName (mkBSS sym)

    pushSymTab
    (paramOpTypes, paramNames) <- fmap unzip $ forM params $ \(S.Param p s t) -> do
        checkSymKeyUndef s KeyVar
        opTyp <- opTypeOf t
        let paramName = mkBSS s
        return (opTyp, ParameterName paramName)

    returnOpType <- maybe (return VoidType) opTypeOf mretty

    -- InstrCmpT { getInstrCmp :: IRBuilderT (ModuleCmpT s m) a }
--    op <- function name (zip paramOpTypes paramNames) returnOpType $ \argOp -> do
--        getInstrCmp (mapM_ cmpStmt blk)
    
    popSymTab

    --addObj sym (KeyFunc paramTypes) (ObjFunc mretty op) 
    addDeclaration sym (KeyFunc paramTypes) (DecFunc name paramOpTypes returnOpType)


cmpStmt :: InsCmp CompileState m => S.Stmt -> m ()
cmpStmt stmt = case stmt of
    _ -> fail (show stmt)


opTypeOf :: ModCmp CompileState m => T.Type -> m Type
opTypeOf typ = case typ of
    T.I64       -> return i64
    T.I32       -> return i32
    T.Bool      -> return i1
    T.Tuple ts  -> fmap (StructureType False) (mapM opTypeOf ts)
    T.Typedef s -> do
        ObType t nm <- look s KeyType
        case nm of
            Nothing -> opTypeOf t
            Just n  -> return (NamedTypeReference n)
    _ -> error (show typ) 



prettyCompileState :: CompileState -> IO ()
prettyCompileState state = do
    putStrLn "defs:"
    forM_ (definitions state) $ \d ->
        putStrLn $ take 200 (show d)
