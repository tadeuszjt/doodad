{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CGenerate where

import Data.List
import Data.Char
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Symbol
import ASTResolved hiding (moduleName)
import qualified ASTResolved
import CBuilder as C hiding (moduleName)
import CAst as C
import Control.Monad.State
import Type
import AST as S
import Error
import qualified SymTab
import FunctionFinder

data Value
    = Value { valType :: Type.Type, valExpr :: C.Expression }
    | Ref   { refType :: Type.Type, refExpr :: C.Expression }
    deriving (Show, Eq)

instance Typeof Value where
    typeof (Value t _) = t
    typeof (Ref t _)   = t


data GenerateState
    = GenerateState
        { moduleName  :: String
        , structs      :: Map.Map C.Type String
        , supply      :: Map.Map String Int
        , curFnIsRef  :: Bool
        , symTab      :: SymTab.SymTab String Value
        , astResolved :: ASTResolved
        }

initGenerateState ast
    = GenerateState
        { moduleName = ASTResolved.moduleName ast
        , structs = Map.empty
        , supply = Map.empty
        , symTab = SymTab.initSymTab
        , curFnIsRef = False
        , astResolved = ast
        }

newtype Generate a = Generate { unGenerate :: StateT GenerateState (StateT BuilderState (ExceptT Error IO)) a }
    deriving (Functor, Applicative, Monad, MonadState GenerateState, MonadIO, MonadError Error)

instance MonadBuilder Generate where
    liftBuilderState (StateT s) = Generate $ lift $ StateT $ pure . runIdentity . s

instance MonadFail Generate where
    fail s = throwError (ErrorStr s)

instance TypeDefs Generate where
    getTypeDefs = gets (typeDefsAll . astResolved)



runGenerate :: MonadIO m => GenerateState -> BuilderState -> Generate a -> m (Either Error ((a, GenerateState), BuilderState))
runGenerate generateState builderState generate =
    liftIO $ runExceptT $ runStateT (runStateT (unGenerate generate) generateState) builderState


pushSymTab :: Generate ()
pushSymTab = modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: Generate ()
popSymTab = modify $ \s -> s { symTab = SymTab.pop (symTab s) }


define :: String -> Value -> Generate ()
define str obj = do
    resm <- SymTab.lookup str <$> gets symTab
    check (isNothing resm) $ str ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert str obj (symTab s) }


look :: String -> Generate Value
look str = do
    resm <- SymTab.lookup str <$> gets symTab
    when (isNothing resm) $ fail $ str ++ " isn't defined"
    return (fromJust resm)


fresh :: String -> Generate String
fresh suggestion = do
    nm <- Map.lookup suggestion <$> gets supply
    let n = maybe 0 id nm
    modify $ \s -> s { supply = Map.insert suggestion (n + 1) (supply s) }
    return $ suggestion ++ show n


true :: Value
true = Value Type.Bool (C.Bool True)


false :: Value
false = Value Type.Bool (C.Bool False)


i64 :: Int -> Value
i64 n = Value I64 (C.Int $ fromIntegral n)

not_ :: Value -> Value
not_ (Value typ expr) = Value typ (C.Not expr)


greaterEqual :: Value -> Value -> Generate Value
greaterEqual a@(Value _ _) b@(Value _ _) = do
    unless (typeof a == typeof b) (error "type mismatch")
    base <- baseTypeOf a
    case base of
        I64 -> return $ Value Type.Bool $ C.Infix C.GTEq (valExpr a) (valExpr b)
        x -> error (show x)


callFunction :: Symbol -> Type.Type -> [Value] -> Generate Value
callFunction symbol retty args = do
    ast <- gets astResolved

    callSymbol <- case symbolIsResolved symbol of
        True -> return symbol
        False -> do
            funcSymbolm <- findInstance ast $ CallHeader symbol (map typeof args) retty
            unless (isJust funcSymbolm)
                (error $ "couldn't find instance: " ++ show (prettySymbol symbol, retty, map typeof args))
            return (fromJust funcSymbolm)

    let header = getInstanceHeader callSymbol ast
    let funcParams = S.funcArgs header
    let funcRetty = S.funcRetty header

    unless (length funcParams == length args) (error "invalid number of args")

    argExprs <- forM (zip args funcParams) $ \(arg, param) -> case (arg, param) of
        (Value _ _, S.Param _ _ _) -> return (valExpr arg)
        (Ref _ _, S.RefParam _ _ _) -> return (refExpr arg)
        (Ref _ _, S.Param _ _ _) -> valExpr <$> deref arg
        (Value _ _, S.RefParam _ _ _) -> refExpr <$> reference arg
        x -> error (show x)

    case funcRetty of
        Retty Void -> do
            appendElem $ C.ExprStmt $ C.Call (showSymGlobal callSymbol) argExprs
            return $ Value Void (C.Int 0)
        Retty retType    -> assign "call" $ Value retType $ C.Call (showSymGlobal callSymbol) argExprs
        RefRetty retType -> assign "call" $ Ref retType $ C.Call (showSymGlobal callSymbol) argExprs


assign :: String -> Value -> Generate Value
assign suggestion val = do
    name <- fresh suggestion
    case val of
        Value _ _ -> do
            ctyp <- cTypeOf (typeof val)
            appendElem $ C.Assign ctyp name (valExpr val)
            return $ Value (typeof val) $ C.Ident name
        Ref _ _ -> do
            ctyp <- cRefTypeOf (typeof val)
            appendElem $ C.Assign ctyp name (refExpr val)
            return $ Ref (typeof val) $ C.Ident name


if_ :: Value -> Generate a -> Generate a
if_ cnd f = do
    base@(Type.Bool) <- baseTypeOf cnd
    id <- appendIf (valExpr cnd)
    withCurID id f


call :: String -> [Value] -> Generate () 
call name args = do
    void $ appendElem $ C.ExprStmt $ C.Call name (map valExpr args)


for :: Value -> (Value -> Generate a) -> Generate a
for len f = do
    base@(I64) <- baseTypeOf len
    idx <- assign "idx" (i64 0)
    id <- appendElem $ C.For
        Nothing
        (Just $ C.Infix C.LT (valExpr idx) (valExpr len))
        (Just $ C.Increment $ valExpr idx)
        []
    withCurID id (f idx)


reference :: Value -> Generate Value
reference val = do
    base <- baseTypeOf val
    case val of
        Ref _ _ -> return val
        Value _ _ -> case base of
            TypeApply (Sym ["Tuple"]) ts -> do
                ref <- assign "ref" $ Ref (typeof val) $ C.Initialiser [C.Address (valExpr val), C.Int 0, C.Int 0]
                return ref
            _ -> return $ Ref (typeof val) (C.Address $ valExpr val)
            x -> error (show x)


deref :: Value -> Generate Value
deref (Value t e) = return (Value t e)
deref (Ref typ expr) = do
    base <- baseTypeOf typ
    case base of
        x | isSimple x -> return $ Value typ (C.Deref expr)
        TypeApply (Sym ["Sum"]) _ -> return $ Value typ (C.Deref expr)
        TypeApply (Sym ["Array"]) _ -> return $ Value typ (C.Deref expr)
        TypeApply (Sym ["Tuple"]) ts  -> do
            -- TODO implement memory shear
            let ptr = C.Member expr "ptr"
            return $ Value typ (C.Deref ptr)

        x -> error (show x)
    

member :: Int -> Value -> Generate Value
member idx (Ref typ expr) = do
    base <- baseTypeOf typ
    case base of
        TypeApply (Sym ["Tuple"]) ts -> do
            unless (idx >= 0 && idx < length ts) (error "invalid member index")
            -- TODO implement shear
            return $ Value (ts !! idx) $ C.PMember (C.Member expr "ptr") ("m" ++ show idx)
        TypeApply (Sym ["Sum"]) ts   -> do
            unless (idx >= 0 && idx < length ts) (error "invalid Sum field index")
            return $ Value (ts !! idx) $ C.PMember expr ("u" ++ show idx)
        x -> error (show x)
member idx val = do
    base <- baseTypeOf val
    case base of
        TypeApply (Sym ["Tuple"]) ts -> do
            unless (idx >= 0 && idx < length ts) (error "invalid member index")
            return $ Value (ts !! idx) $ C.Member (valExpr val) ("m" ++ show idx)

        TypeApply (Sym ["Table"]) t -> do
            error ""

        TypeApply (Sym ["Sum"]) ts   -> do
            unless (idx >= 0 && idx < length ts) (error "invalid Sum field index")
            return $ Value (ts !! idx) $ C.Member (valExpr val) ("u" ++ show idx)
        _ -> error (show base)



isCopyable :: Type.Type -> Generate Bool
isCopyable typ = do
    base <- baseTypeOf typ
    case base of
        x | isSimple x                        -> return True
        TypeApply (Sym ["Tuple"]) ts          -> all id <$> mapM isCopyable ts
        TypeApply (Sym ["Sum"])   ts          -> all id <$> mapM isCopyable ts
        TypeApply (Sym ["Array"]) [t, Size n] -> isCopyable t
        x -> error (show x)


cParamOf :: S.Param -> Generate C.Param
cParamOf param = do
    ctype <- case param of
        S.Param _ _ _ -> cTypeOf param
        S.RefParam _ _ _ -> cRefTypeOf param
    return $ C.Param { C.cName = showSymLocal (paramSymbol param), C.cType = ctype }


cRefTypeOf :: Typeof a => a -> Generate C.Type
cRefTypeOf a = do
    base <- baseTypeOf a
    case base of
        x | isSimple x -> Cpointer <$> cTypeOf a 
        TypeApply (Sym ["Table"]) _  -> Cpointer <$> cTypeOf a
        TypeApply (Sym ["Sum"]) ts   -> Cpointer <$> cTypeOf a
        TypeApply (Sym ["Array"]) ts -> Cpointer <$> cTypeOf a
        TypeApply (Sym ["Tuple"]) ts -> do
            pt <- Cpointer <$> cTypeOf a
            cst <- return $ Cstruct [C.Param "ptr" pt, C.Param "idx" Csize_t, C.Param "cap" Csize_t]
            getTypedef "Ref" cst

        Slice t -> Cpointer <$> cTypeOf a

        x -> error (show x)


cTypeOf :: (Typeof a) => a -> Generate C.Type
cTypeOf a = case typeof a of
    I64            -> return Cint64_t
    I32            -> return Cint32_t
    I8             -> return Cint8_t
    U8             -> return Cuint8_t
    F64            -> return Cdouble
    F32            -> return Cfloat
    Void           -> return Cvoid
    Type.Bool      -> return Cbool
    Type.Char      -> return Cchar

    Type.Slice t              -> getTypedef "Slice"  =<< cTypeNoDef (Type.Slice t)
    TypeApply (Sym ["Tuple"]) t -> getTypedef "Tuple"  =<< cTypeNoDef (TypeApply (Sym ["Tuple"]) t)
    TypeApply (Sym ["Table"]) t -> getTypedef "Table"  =<< cTypeNoDef (TypeApply (Sym ["Table"]) t)
    TypeApply (Sym ["Sum"]) ts  -> getTypedef "Sum"    =<< cTypeNoDef (TypeApply (Sym ["Sum"]) ts)
    TypeApply (Sym ["Array"]) ts -> do
        let [t, Size n] = ts
        getTypedef "Array" =<< cTypeNoDef (TypeApply (Sym ["Array"]) ts)

    TypeApply symbol args -> do
        (generics, typ) <- (Map.! symbol) <$> getTypeDefs
        getTypedef (Symbol.sym symbol) =<< cTypeOf =<< applyTypeArguments generics args typ

    _ -> error (show $ typeof a)

    where
        cTypeNoDef :: (Typeof a) => a -> Generate C.Type
        cTypeNoDef a = case typeof a of
            I64 -> return Cint64_t
            I32 -> return Cint32_t
            I8 ->  return Cint8_t
            U8 ->  return Cuint8_t
            F64 -> return Cdouble
            F32 -> return Cfloat
            Void -> return Cvoid
            Type.Bool -> return Cbool
            Type.Char -> return Cchar
            TypeApply (Sym ["Tuple"]) ts -> do
                cts <- mapM cTypeOf ts
                return $ Cstruct $ map (\(ct, i) -> C.Param ("m" ++ show i) ct) (zip cts [0..])

            TypeApply (Sym ["Sum"]) ts -> do
                cts <- mapM cTypeOf ts
                return $ Cstruct [C.Param "en" Cint64_t, C.Param "" $
                    Cunion $ map (\(ct, i) -> C.Param ("u" ++ show i) ct) (zip cts [0..])]

            TypeApply (Sym ["Table"]) [t] -> do
                baseT <- baseTypeOf t
                cts <- mapM cTypeOf =<< case baseT of
                    t              -> return [t]
                let pts = zipWith (\ct i -> C.Param ("r" ++ show i) (Cpointer ct)) cts [0..]
                return $ Cstruct (C.Param "len" Cint64_t:C.Param "cap" Cint64_t:pts)

            TypeApply (Sym ["Array"]) [t, Size n] -> do
                cType <- cTypeOf t
                return $ Cstruct [C.Param "arr" (Carray n cType) ]

            Type.Slice t -> do
                cType <- cTypeOf t
                return $ Cstruct [C.Param "ptr" (Cpointer cType), C.Param "len" Csize_t]


            x -> error (show x)


getTypedef :: String -> C.Type -> Generate C.Type
getTypedef suggestion typ = do
    sm <- Map.lookup typ <$> gets structs
    case sm of
        Just s -> return $ Ctypedef s
        Nothing -> do
            name <- fresh suggestion
            appendTypedef typ name
            modify $ \s -> s { structs = Map.insert typ name (structs s) }
            return $ Ctypedef name
