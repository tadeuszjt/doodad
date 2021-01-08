{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
    deriving (Show, Eq, Ord)


data Object
    = ObType    T.Type   (Maybe Name)
    | ObjExtern [T.Type] (Maybe T.Type) Operand
    | ObjFunc   (Maybe T.Type) Operand
    | ObjVal    Value
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
