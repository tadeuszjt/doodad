{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CGenerate where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map
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


genSymbol :: Symbol -> ASTResolved -> Generate (Symbol, ASTResolved)
genSymbol symbol@(SymResolved str) ast = do  
    let modName = ASTResolved.moduleName ast
    let im = Map.lookup symbol (symSupply ast)
    let n = maybe 0 (id) im
    let ast' = ast { symSupply = Map.insert symbol (n + 1) (symSupply ast) }
    return (SymResolved ([modName] ++ str ++ [show n]), ast')


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
pushSymTab = do
    --liftIO $ putStrLn "pushSymTab"
    modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: Generate ()
popSymTab = do
    --liftIO $ putStrLn "popSymTab"
    modify $ \s -> s { symTab = SymTab.pop (symTab s) }

define :: String -> Value -> Generate ()
define str obj = do
    --liftIO $ putStrLn ("defining: " ++ show str)
    resm <- SymTab.lookupHead str <$> gets symTab
    check (isNothing resm) $ str ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert str obj (symTab s) }


look :: String -> Generate Value
look str = do
    resm <- SymTab.lookupHead str <$> gets symTab
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
    error "callFunction"
--    ast <- gets astResolved
--
--    callSymbol <- case symbolIsResolved symbol of
--        True -> return symbol
--        False -> do
--            funcSymbolm <- findInstance ast symbol $ Apply Type.Func (retty : map typeof args)
--            unless (isJust funcSymbolm)
--                (error $ "couldn't find instance: " ++ show (prettySymbol symbol, retty, map typeof args))
--            return (fromJust funcSymbolm)
--
--    let header = getInstanceHeader callSymbol ast
--    let funcParams = S.funcArgs header
--    let funcRetty = S.funcRetty header
--
--    unless (length funcParams == length args) (error "invalid number of args")
--
--    argExprs <- forM (zip args funcParams) $ \(arg, param) -> case (arg, param) of
--        (Value _ _, S.Param _ _ _) -> return (valExpr arg)
--        (Ref _ _, S.RefParam _ _ _) -> return (refExpr arg)
--        (Ref _ _, S.Param _ _ _) -> valExpr <$> deref arg
--        (Value _ _, S.RefParam _ _ _) -> refExpr <$> reference arg
--        x -> error (show x)
--
--    case funcRetty of
--        Retty Void -> do
--            appendElem $ C.ExprStmt $ C.Call (showSymGlobal callSymbol) argExprs
--            return $ Value Void (C.Int 0)
--        Retty retType    -> assign "call" $ Value retType $ C.Call (showSymGlobal callSymbol) argExprs
--        RefRetty retType -> assign "call" $ Ref retType $ C.Call (showSymGlobal callSymbol) argExprs
--

initialiser :: Type.Type -> Generate C.Expression
initialiser typ = do
    b <- hasNonZero typ
    case b of
        True -> return $ C.Initialiser [C.Int 0]
        False -> return $ C.Initialiser []
    where
        hasNonZero :: Type.Type -> Generate Bool
        hasNonZero typ = do
            base <- baseTypeOf typ
            case unfoldType base of
                (x, []) | isSimple x -> return True

                (Tuple, []) -> return False
                (Tuple, ts) -> any id <$> mapM hasNonZero ts

                (Type.Array, _) -> return True
                (Type.Sum, _) -> return True
                (Type.Table, _) -> return True

                x -> error (show x)


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
        Value _ _ -> case unfoldType base of
            (Tuple, ts) -> do
                ref <- assign "ref" $ Ref (typeof val) $ C.Initialiser [C.Address (valExpr val), C.Int 0, C.Int 0]
                return ref
            _ -> return $ Ref (typeof val) (C.Address $ valExpr val)
            x -> error (show x)


deref :: Value -> Generate Value
deref (Value t e) = return (Value t e)
deref (Ref typ expr) = do
    base <- baseTypeOf typ
    case unfoldType base of
        (Tuple, ts) -> do
            -- TODO implement memory shear
            let ptr = C.Member expr "ptr"
            return $ Value typ (C.Deref ptr)

        _ -> return $ Value typ (C.Deref expr)
    

member :: Int -> Value -> Generate Value
member idx (Ref typ expr) = do
    base <- baseTypeOf typ
    case unfoldType base of
        (Tuple, ts) -> do
            unless (idx >= 0 && idx < length ts) (error "invalid member index")
            -- TODO implement shear
            return $ Value (ts !! idx) $ C.PMember (C.Member expr "ptr") ("m" ++ show idx)
        (Sum, ts) -> do
            unless (idx >= 0 && idx < length ts) (error "invalid Sum field index")
            return $ Value (ts !! idx) $ C.PMember expr ("u" ++ show idx)
        x -> error (show x)
member idx val = do
    base <- baseTypeOf val
    case unfoldType base of
        (Tuple, ts) -> do
            unless (idx >= 0 && idx < length ts) (error "invalid member index")
            return $ Value (ts !! idx) $ C.Member (valExpr val) ("m" ++ show idx)
--
--        TypeApply (Sym ["Table"]) t -> do
--            error ""
--
        (Sum, ts) -> do
            unless (idx >= 0 && idx < length ts) (error "invalid Sum field index")
            return $ Value (ts !! idx) $ C.Member (valExpr val) ("u" ++ show idx)
        x -> error (show x)



isCopyable :: Type.Type -> Generate Bool
isCopyable typ = do
    base <- baseTypeOf typ
    case base of
        x | isSimple x                        -> return True
        x -> error (show x)
--        TypeApply (Sym ["Tuple"]) ts          -> all id <$> mapM isCopyable ts
--        TypeApply (Sym ["Sum"])   ts          -> all id <$> mapM isCopyable ts
--        TypeApply (Sym ["Array"]) [t, Size n] -> isCopyable t
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
    case unfoldType base of
        (x, []) | isSimple x -> Cpointer <$> cTypeOf a
        (Table, _)           -> Cpointer <$> cTypeOf a
        (Slice, _)           -> Cpointer <$> cTypeOf a
        (Sum, _)             -> Cpointer <$> cTypeOf a
        (Type.Array, _)      -> Cpointer <$> cTypeOf a

        (Tuple, ts) -> do
            pt <- Cpointer <$> cTypeOf a
            cst <- return $ Cstruct [C.Param "ptr" pt, C.Param "idx" Csize_t, C.Param "cap" Csize_t]
            getTypedef "Ref" cst

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
    Type.Tuple     -> getTypedef "Tuple" (Cstruct [])

    TypeDef s -> do
        ([], typ) <- (Map.! s) <$> getTypeDefs
        getTypedef (Symbol.sym s) =<< cTypeOf typ

    Apply Type.Slice t -> getTypedef "Slice" =<< cTypeNoDef (typeof a)
    Apply Table _ -> getTypedef "Table" =<< cTypeNoDef (typeof a)

    x@(Apply _ _) -> case unfoldType x of
        (Tuple, _)      -> getTypedef "Tuple" =<< cTypeNoDef (typeof a)
        (Sum, _)        -> getTypedef "Sum" =<< cTypeNoDef (typeof a)
        (Type.Array, _) -> getTypedef "Array" =<< cTypeNoDef (typeof a)

        (TypeDef s, ts) -> do
            (generics, typ) <- (Map.! s) <$> getTypeDefs
            getTypedef (Symbol.sym s) =<< cTypeOf =<< applyTypeM generics ts typ


        x -> error (show x)


--    Apply Tuple _ -> getTypedef "Tuple" =<< cTypeNoDef (typeof a)
--    Apply Type.Array t -> getTypedef "Array" =<< cTypeNoDef (typeof a)


    x -> error (show x)

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

            Apply Type.Slice t -> do
                cType <- cTypeOf t
                return $ Cstruct [C.Param "ptr" (Cpointer cType), C.Param "len" Csize_t]

            Apply Table t -> do
                baseT <- baseTypeOf t
                cts <- mapM cTypeOf =<< case baseT of
                    t              -> return [t]
                let pts = zipWith (\ct i -> C.Param ("r" ++ show i) (Cpointer ct)) cts [0..]
                return $ Cstruct (C.Param "len" Cint64_t:C.Param "cap" Cint64_t:pts)


            Apply (Apply Type.Array (Size n)) t -> do
                cType <- cTypeOf t
                return $ Cstruct [C.Param "arr" (Carray n cType) ]

            x@(Apply _ _) -> case unfoldType x of
                (Tuple, ts) -> do
                    cts <- mapM cTypeOf ts
                    return $ Cstruct $ map (\(ct, i) -> C.Param ("m" ++ show i) ct) (zip cts [0..])
                
                (Sum, ts) -> do
                    cts <- mapM cTypeOf ts
                    return $ Cstruct [C.Param "en" Cint64_t, C.Param "" $
                        Cunion $ map (\(ct, i) -> C.Param ("u" ++ show i) ct) (zip cts [0..])]
                x -> error (show x)


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
