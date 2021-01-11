{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module CompileState where

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

import qualified AST as S
import Monad
import Error
import qualified SymTab
import qualified Type as T


data Value
    = Val { valType :: T.Type, valOp :: Operand }
    | Ptr { valType :: T.Type, valOp :: Operand }
    deriving (Show, Eq)


data SymKey
    = KeyType
    | KeyVar
    | KeyFunc [T.Type]
    | KeyExtern
    deriving (Show, Eq, Ord)


data Object
    = ObjVal    Value
    | ObType    T.Type         (Maybe Name)
    | ObjFunc   (Maybe T.Type) Operand
    | ObjExtern [T.Type]       (Maybe T.Type) Operand
    deriving (Show)


data Declaration
    = DecType   
    | DecExtern [Type] Type Bool
    | DecFunc   [Type] Type
    deriving (Show)


data CompileState
    = CompileState
        { imports      :: Map.Map S.ModuleName CompileState
        , decMap       :: Map.Map (S.Symbol, SymKey) Name
        , declarations :: Map.Map Name Declaration
        , declared     :: Set.Set Name
        , definitions  :: [Definition]
        , symTab       :: SymTab.SymTab S.Symbol SymKey Object
        }
    deriving (Show)

initCompileState
     = CompileState
        { imports      = Map.empty
        , decMap       = Map.empty
        , declarations = Map.empty
        , declared     = Set.empty
        , definitions  = []
        , symTab       = SymTab.initSymTab
        }


assert :: MonadError CmpError m => Bool -> String -> m ()
assert b s =
    unless b $ throwError $ CmpError (Nothing, s)


addObj :: BoM CompileState m => S.Symbol -> SymKey -> Object -> m ()
addObj sym key obj =
    modify $ \s -> s { symTab = SymTab.insert sym key obj (symTab s) }


checkSymKeyUndef :: BoM CompileState m => S.Symbol -> SymKey -> m ()
checkSymKeyUndef sym key = do
    res <- fmap (SymTab.lookupSymKey sym key) (gets symTab)
    when (isJust res) $ fail (sym ++ " already defined")


checkSymUndef :: BoM CompileState m => S.Symbol -> m ()
checkSymUndef sym = do
    res <- fmap (SymTab.lookupSym sym) (gets symTab)
    when (isJust res) $ fail (sym ++ " already defined")


addDeclared :: BoM CompileState m => Name -> m ()
addDeclared name =
    modify $ \s -> s { declared = Set.insert name (declared s) }


addSymKeyDec :: BoM CompileState m => S.Symbol -> SymKey -> Name -> Declaration -> m ()
addSymKeyDec sym key name dec = do
    modify $ \s -> s { decMap = Map.insert (sym, key) name (decMap s) }
    modify $ \s -> s { declarations = Map.insert name dec (declarations s) }


addDeclaration :: BoM CompileState m => Name -> Declaration -> m ()
addDeclaration name dec = do
    modify $ \s -> s { declarations = Map.insert name dec (declarations s) }


emitDec :: ModCmp CompileState m => Name -> Declaration -> m ()
emitDec name dec = case dec of
    DecType                         -> void (typedef name Nothing)
    DecFunc argTypes retty          -> emitDec name (DecExtern argTypes retty False)
    DecExtern argTypes retty isVarg -> do
        emitDefn $ GlobalDefinition $ functionDefaults
            { returnType = retty
            , name       = name
            , parameters = ([Parameter typ (mkName "") [] | typ <- argTypes], isVarg)
            }
        

ensureDec :: ModCmp CompileState m => Name -> m ()
ensureDec name = do
    declared <- fmap (Set.member name) (gets declared)
    when (not declared) $ do
        res <- fmap (Map.lookup name) (gets declarations)
        case res of
            Nothing -> return ()
            Just d  -> emitDec name d >> addDeclared name


ensureSymKeyDec :: ModCmp CompileState m => S.Symbol -> SymKey -> m ()
ensureSymKeyDec sym key = do
    nm <- fmap (Map.lookup (sym, key)) (gets decMap)
    case nm of
        Just name -> ensureDec name
        Nothing   -> do
            imports <- fmap Map.elems (gets imports)
            rs <- fmap catMaybes $ forM imports $ \imp -> do
                let res = Map.lookup (sym, key) (decMap imp) 
                case res of
                    Nothing   -> return Nothing
                    Just name -> do
                        declared <- fmap (Set.member name) (gets declared)
                        when (not declared) $ emitDec name ((Map.! name) $ declarations imp)
                        return (Just ())

            case rs of
                []    -> return ()
                [r]   -> return ()
                (r:_) -> fail ("more than one declaration for: " ++ sym ++ " " ++ show key)


ensureExtern :: ModCmp CompileState m => Name -> [Type] -> Type -> Bool -> m Operand
ensureExtern name argTypes retty isVarg = do
    declared <- fmap (Set.member name) (gets declared)
    when (not declared) $ do
        addDeclaration name (DecExtern argTypes retty isVarg)
        ensureDec name
    
    return $ ConstantOperand $
        GlobalReference (ptr $ FunctionType retty argTypes isVarg) name



look :: ModCmp CompileState m => S.Symbol -> SymKey -> m Object
look sym key = do
    ensureSymKeyDec sym key
    res <- fmap (SymTab.lookupSymKey sym key) (gets symTab)
    case res of
        Just obj -> return obj
        Nothing  -> do
            r <- fmap (catMaybes . map (SymTab.lookupSymKey sym key . symTab) . Map.elems) (gets imports)
            case r of
                [] -> error ("no obj for: " ++ sym)
                [x] -> return x


pushSymTab :: BoM CompileState m => m ()
pushSymTab =
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: BoM CompileState m => m ()
popSymTab =
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }
