{-# LANGUAGE FlexibleContexts #-}
module CompileState where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)

import qualified LLVM.AST.Constant as C
import qualified LLVM.Internal.FFI.DataLayout as FFI
import LLVM.AST hiding (function, type')
import LLVM.AST.Global
import LLVM.AST.Constant as C hiding (type')
import LLVM.AST.Type hiding (void)
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Context
import Foreign.Ptr

import qualified AST as S
import qualified Type as T
import qualified SymTab
import qualified JIT
import Monad
import Error




data Value
    = Val { valType :: T.Type, valOp :: Operand }
    | Ptr { valType :: T.Type, valLoc :: Operand }
    | CtxTable [[Value]]
    | CtxTuple [Value]
    deriving (Show, Eq)


data SymKey
    = KeyType
    | KeyVar
    | KeyFunc [T.Type]
    | KeyExtern
    deriving (Show, Eq, Ord)


data Object
    = ObjVal         Value
    | ObType         T.Type   (Maybe Name)
    | ObjFunc        T.Type   Operand
    | ObjExtern      [T.Type] T.Type Operand
    | ObjConstructor T.Type
    deriving (Show)



data Declaration
    = DecType   
    | DecExtern [Type] Type Bool
    | DecFunc   [Type] Type
    | DecVar    Type
    deriving (Show)


data CompileState
    = CompileState
        { context      :: Context
        , dataLayout   :: Ptr FFI.DataLayout
        , imports      :: Map.Map S.Path CompileState
        , decMap       :: Map.Map (S.Symbol, SymKey) Name
        , declarations :: Map.Map Name Declaration
        , declared     :: Set.Set Name
        , definitions  :: [Definition]
        , symTab       :: SymTab.SymTab S.Symbol SymKey Object
        , curRetType   :: T.Type
        , posStack     :: [TextPos]
        }

initCompileState ctx dl imports
     = CompileState
        { context      = ctx
        , dataLayout   = dl
        , imports      = imports
        , decMap       = Map.empty
        , declarations = Map.empty
        , declared     = Set.empty
        , definitions  = []
        , symTab       = SymTab.initSymTab
        , curRetType   = T.Void
        , posStack     = [TextPos "" 0 0 0]
        }


assert :: BoM CompileState m => Bool -> String -> m ()
assert b s = do
    pos <- fmap head (gets posStack)
    unless b $ throwError (ErrorFile pos s)


withPos :: BoM CompileState m => TextPos -> m a -> m a
withPos pos f = do
    modify $ \s -> s { posStack = pos:(posStack s) }
    r <- f
    modify $ \s -> s { posStack = tail (posStack s) }
    return r


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
    DecVar opTyp                    -> do
        emitDefn $ GlobalDefinition $ globalVariableDefaults
            { name  = name
            , type' = opTyp
            }
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
                []  -> fail ("no obj for: " ++ sym)
                [x] -> return x


pushSymTab :: BoM CompileState m => m ()
pushSymTab =
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: BoM CompileState m => m ()
popSymTab =
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }


prettyCompileState :: CompileState -> IO ()
prettyCompileState state = do
    putStrLn "defs:"
    forM_ (definitions state) $ \def -> case def of
        TypeDefinition name mtyp ->
            putStrLn ("type: " ++ show name ++ " " ++ show mtyp)
        GlobalDefinition (Function _ _ _ _ _ retty name params _ _ _ _ _ _ basicBlocks _ _) -> do
            let ps = concat (map show $ fst params)
            putStrLn ("func: " ++ show name ++ " " ++ ps ++ " " ++ show retty)
        _ -> return ()
