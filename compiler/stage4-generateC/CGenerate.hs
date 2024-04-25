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
        , tuples      :: Map.Map C.Type String
        , supply      :: Map.Map String Int
        , typefuncs   :: Map.Map Symbol ([Symbol], Type.Type)
        , refFuncs    :: Map.Map Symbol Bool
        , refFuncArgs :: Map.Map (Symbol, Int) Bool
        , curFnIsRef  :: Bool
        , symTab      :: SymTab.SymTab String () Value
        }

initGenerateState modName
    = GenerateState
        { moduleName = modName
        , tuples = Map.empty
        , supply = Map.empty
        , typefuncs = Map.empty
        , symTab = SymTab.initSymTab
        , refFuncs = Map.empty
        , refFuncArgs = Map.empty
        , curFnIsRef = False
        }

newtype Generate a = Generate { unGenerate :: StateT GenerateState (StateT BuilderState (ExceptT Error IO)) a }
    deriving (Functor, Applicative, Monad, MonadState GenerateState, MonadIO, MonadError Error)

instance MonadBuilder Generate where
    liftBuilderState (StateT s) = Generate $ lift $ StateT $ pure . runIdentity . s

instance MonadFail Generate where
    fail s = throwError (ErrorStr s)

instance TypeDefs Generate where
    getTypeDefs = gets typefuncs


runGenerate :: MonadIO m => GenerateState -> BuilderState -> Generate a -> m (Either Error ((a, GenerateState), BuilderState))
runGenerate generateState builderState generate =
    liftIO $ runExceptT $ runStateT (runStateT (unGenerate generate) generateState) builderState


pushSymTab :: Generate ()
pushSymTab = modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: Generate ()
popSymTab = modify $ \s -> s { symTab = SymTab.pop (symTab s) }


addFuncRefType :: Symbol -> Bool -> Generate ()
addFuncRefType symbol b = do
    modify $ \s -> s { refFuncs = Map.insert symbol b (refFuncs s) }


getFuncRefType :: Symbol -> Generate Bool
getFuncRefType symbol = do
    refFuncs <- gets refFuncs
    mapGet symbol refFuncs


define :: String -> Value -> Generate ()
define str obj = do
    resm <- SymTab.lookup str () <$> gets symTab
    check (isNothing resm) $ str ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert str () obj (symTab s) }


look :: String -> Generate Value
look str = do
    resm <- SymTab.lookup str () <$> gets symTab
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


equalEqual :: Value -> Value -> Generate Value
equalEqual a@(Value _ _) b@(Value _ _) = do
    unless (typeof a == typeof b) (error "type mismatch")
    base <- baseTypeOf a
    return $ Value Type.Bool $ case base of
        x | isSimple x -> C.Infix C.EqEq (valExpr a) (valExpr b)

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


convert :: Type.Type -> Value -> Generate Value
convert typ val = do
    base <- baseTypeOf typ
    case base of
        I64 -> case val of
            Value t e -> do
                baseVal <- baseTypeOf t
                case baseVal of
                    I32 -> return $ Value typ e
                    I64 -> return $ Value typ e
                    x -> error (show x)

        TypeApply (Sym "Sum") ts -> do
            case val of
                val -> do
                    let Just idx = elemIndex (typeof val) ts
                    idx <- case elemIndex (typeof val) ts of
                        Nothing -> error $ show (typeof val) ++ " not found in: " ++ show typ
                        Just idx -> return idx

                        
                    sum <- assign "sum" $ Value typ (C.Initialiser [C.Int $ fromIntegral idx])
                    set (Value (typeof val) $ C.Member (valExpr sum) ("u" ++ show idx)) val
                    return sum

        x -> error (show x)

deref :: Value -> Generate Value
deref (Value t e) = return (Value t e)
deref (Ref typ expr) = do
    base <- baseTypeOf typ
    case base of
        x | isSimple x -> return $ Value typ (C.Deref expr)
        TypeApply (Sym "Sum") _ -> return $ Value typ (C.Deref expr)
        TypeApply (Sym "Array") _ -> return $ Value typ (C.Deref expr)
        TypeApply (Sym "Tuple") ts  -> do
            -- TODO implement memory shear
            let ptr = C.Member expr "ptr"
            return $ Value typ (C.Deref ptr)

        x -> error (show x)
    

set :: Value -> Value -> Generate ()
set a b = do
    unless (typeof a == typeof b) $ error "set: type mismatch"
    let typ = typeof a
    base <- baseTypeOf (typeof a)
    copyable <- isCopyable (typeof a)
    case (a, b) of
        (Value _ a, Value _ b) | copyable -> void $ appendElem (C.Set a b)
        (Value _ a, Ref tb b) -> do
            valB <- deref (Ref tb b)
            if copyable then
                void $ appendElem $ C.Set a (valExpr valB)
            else do
                error "here"

        (Value _ a, Value _ b) -> case base of
            TypeApply (Sym "Tuple") ts -> do
                forM_ (zip ts [0..]) $ \(t, i) -> do
                    let va = Value t $ C.Member a ("m" ++ show i)
                    let vb = Value t $ C.Member b ("m" ++ show i)
                    set va vb
            x -> error (show x)
        x -> error (show x)


len :: Value -> Generate Value
len val = case val of
    Ref typ expr -> do
        base <- baseTypeOf typ
        return $ case base of
            TypeApply (Sym "Table") _ -> Value I64 (C.PMember expr "len")

    Value typ expr -> do
        base <- baseTypeOf typ
        return $ case base of
            TypeApply (Sym "Table") _ -> Value I64 (C.Member expr "len")
            Slice t                   -> Value I64 (C.Member expr "len")
    --        Type.Array n t -> return $ Value I64 $ C.Int (fromIntegral n)
            _ -> error (show base)



adtEnum :: Value -> Generate Value
adtEnum val = do
    TypeApply (Sym "Sum") _ <- baseTypeOf val
    case val of
        Value _ expr -> return $ Value I64 $ C.Member expr "en"
        Ref _ expr   -> return $ Value I64 $ C.PMember expr "en"


member :: Int -> Value -> Generate Value
member idx (Ref typ expr) = do
    base <- baseTypeOf typ
    case base of
        TypeApply (Sym "Tuple") ts -> do
            unless (idx >= 0 && idx < length ts) (error "invalid member index")
            -- TODO implement shear
            return $ Value (ts !! idx) $ C.PMember (C.Member expr "ptr") ("m" ++ show idx)
        TypeApply (Sym "Sum") ts   -> do
            unless (idx >= 0 && idx < length ts) (error "invalid Sum field index")
            return $ Value (ts !! idx) $ C.PMember expr ("u" ++ show idx)
        x -> error (show x)
member idx val = do
    base <- baseTypeOf val
    case base of
        TypeApply (Sym "Tuple") ts -> do
            unless (idx >= 0 && idx < length ts) (error "invalid member index")
            return $ Value (ts !! idx) $ C.Member (valExpr val) ("m" ++ show idx)

        TypeApply (Sym "Table") t -> do
            error ""
--            Type.Record ts <- baseTypeOf t
--            Type.RecordTree ns <- getRecordTree t
--            leaves <- getRecordLeaves (ns !! idx)
--            let elems  = map (\(t, i) -> C.Member (valExpr val) ("r" ++ show i)) leaves
--            assign "member" $ Value (TypeApply (Sym "Table") $ ts !! idx) $ C.Initialiser $
--                [ C.Member (valExpr val) "len"
--                , C.Member (valExpr val) "cap"
--                ] ++ elems

        TypeApply (Sym "Sum") ts   -> do
            unless (idx >= 0 && idx < length ts) (error "invalid Sum field index")
            return $ Value (ts !! idx) $ C.Member (valExpr val) ("u" ++ show idx)
        _ -> error (show base)


initialiser :: Type.Type -> [Value] -> Generate Value
initialiser typ [] = do
    base <- baseTypeOf typ
    case base of
        TypeApply (Sym "Tuple") [] -> assign "zero" $ Value typ $ C.Initialiser []
        _ -> assign "zero" $ Value typ $ C.Initialiser [C.Int 0]
initialiser typ vals = do
    base <- baseTypeOf typ
    case base of
        TypeApply (Sym "Tuple") ts -> do
            unless (length vals == length ts) (fail "invalid tuple initialiser")
            assign "zero" $ Value typ $ C.Initialiser (map valExpr vals)

        _ -> error (show base)


builtinArrayAt :: Value -> Value -> Generate Value
builtinArrayAt value idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    TypeApply (Sym "Array") [t, Size n] <- baseTypeOf value
    base <- baseTypeOf t

    case value of
        Value _ expr -> case base of
            TypeApply _ _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.Member expr "arr")
                (valExpr idx)
            TypeApply (Sym "Tuple") _ -> error "TODO"
            x -> error (show x)
        Ref _ expr -> case base of
            x | isSimple x -> return $ Ref t $ C.Address $ C.Subscript
                (C.PMember expr "arr")
                (valExpr idx)
            TypeApply (Sym "Tuple") ts -> error "TODO"
            TypeApply _ _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.PMember expr "arr")
                (valExpr idx)
            x -> error (show x)



builtinSliceAt :: Value -> Value -> Generate Value
builtinSliceAt val idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    Slice t <- baseTypeOf val
    base <- baseTypeOf t

    case val of
        Ref _ exp -> case base of
            Type.Char -> return $ Ref t $ C.Address $ C.Subscript (C.PMember exp "ptr") (valExpr idx)
            x -> error (show x)
        Value _ exp -> case base of
            Type.Char -> return $ Ref t $ C.Address $ C.Subscript (C.Member exp "ptr") (valExpr idx)
            x -> error (show x)

builtinTableAt :: Value -> Value -> Generate Value
builtinTableAt val idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    TypeApply (Sym "Table") [t] <- baseTypeOf val
    base <- baseTypeOf t
    case val of
        Value _ expr -> case base of
            TypeApply (Sym "Tuple") ts -> do
                -- TODO implement shear
                let ptr = C.Address $ C.Subscript (C.Member expr "r0") (valExpr idx)
                assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

            _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.Member expr "r0")
                (valExpr idx)

            x -> error (show x)

        Ref _ expr -> case base of
            TypeApply (Sym "Tuple") ts -> do
                -- TODO implement shear
                let ptr = C.Address $ C.Subscript (C.PMember expr "r0") (valExpr idx)
                assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

            _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.PMember expr "r0")
                (valExpr idx)

            x -> error (show x)
builtinTableAt _ _ = fail "here"


isCopyable :: Type.Type -> Generate Bool
isCopyable typ = do
    base <- baseTypeOf typ
    case base of
        x | isSimple x                           -> return True
        TypeApply (Sym "Tuple") ts          -> all id <$> mapM isCopyable ts
        TypeApply (Sym "Sum")   ts          -> all id <$> mapM isCopyable ts
        TypeApply (Sym "Array") [t, Size n] -> isCopyable t
        x -> error (show x)


cParamOf :: S.Param -> Generate C.Param
cParamOf param = do
    ctype <- case param of
        S.Param _ _ _ -> cTypeOf param
        S.RefParam _ _ _ -> cRefTypeOf param
    return $ C.Param { C.cName = show (paramSymbol param), C.cType = ctype }


cRefTypeOf :: Typeof a => a -> Generate C.Type
cRefTypeOf a = do
    base <- baseTypeOf a
    case base of
        x | isSimple x -> Cpointer <$> cTypeOf a 
        TypeApply (Sym "Table") _  -> Cpointer <$> cTypeOf a
        TypeApply (Sym "Sum") ts   -> Cpointer <$> cTypeOf a
        TypeApply (Sym "Array") ts -> Cpointer <$> cTypeOf a
        TypeApply (Sym "Tuple") ts -> do
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

    Type.Slice t                        -> getTypedef "Slice"  =<< cTypeNoDef (Type.Slice t)
    TypeApply (Sym "Tuple") t      -> getTypedef "Tuple"  =<< cTypeNoDef (TypeApply (Sym "Tuple") t)
    TypeApply (Sym "Table") t      -> getTypedef "Table"  =<< cTypeNoDef (TypeApply (Sym "Table") t)
    TypeApply (Sym "Sum") ts       -> getTypedef "Sum"    =<< cTypeNoDef (TypeApply (Sym "Sum") ts)
    TypeApply (Sym "Array") ts -> do
        let [t, Size n] = ts
        getTypedef "Array" =<< cTypeNoDef (TypeApply (Sym "Array") ts)

    TypeApply symbol args -> do
        (generics, typ) <- mapGet symbol =<< getTypeDefs
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
            TypeApply (Sym "Tuple") ts -> do
                cts <- mapM cTypeOf ts
                return $ Cstruct $ map (\(ct, i) -> C.Param ("m" ++ show i) ct) (zip cts [0..])

            TypeApply (Sym "Sum") ts -> do
                cts <- mapM cTypeOf ts
                return $ Cstruct [C.Param "en" Cint64_t, C.Param "" $
                    Cunion $ map (\(ct, i) -> C.Param ("u" ++ show i) ct) (zip cts [0..])]

            TypeApply (Sym "Table") [t] -> do
                baseT <- baseTypeOf t
                cts <- mapM cTypeOf =<< case baseT of
                    t              -> return [t]
                let pts = zipWith (\ct i -> C.Param ("r" ++ show i) (Cpointer ct)) cts [0..]
                return $ Cstruct (C.Param "len" Cint64_t:C.Param "cap" Cint64_t:pts)

            TypeApply (Sym "Array") [t, Size n] -> do
                cType <- cTypeOf t
                return $ Cstruct [C.Param "arr" (Carray n cType) ]

            Type.Slice t -> do
                cType <- cTypeOf t
                return $ Cstruct [C.Param "ptr" (Cpointer cType), C.Param "len" Csize_t]


            x -> error (show x)


getTypedef :: String -> C.Type -> Generate C.Type
getTypedef suggestion typ = do
    sm <- Map.lookup typ <$> gets tuples
    case sm of
        Just s -> return $ Ctypedef s
        Nothing -> do
            name <- fresh suggestion
            newTypedef typ name
            modify $ \s -> s { tuples = Map.insert typ name (tuples s) }
            return $ Ctypedef name


tableAppend :: Value -> Generate ()
tableAppend (Ref typ expr) = do
    TypeApply (Sym "Table") t <- baseTypeOf typ

    let len    = C.Member (C.Deref expr) "len"
    let newLen = (C.Infix C.Plus len $ C.Int 1)
    let cap    = C.Member (C.Deref expr) "cap"
    
    -- realloc if needed
    if_ (Value Type.Bool $ C.Infix C.GTEq len cap) $ do
        appendElem $ C.Set cap (C.Infix C.Times newLen (C.Int 2))

        let pMem = C.PMember expr "r0"
        let elemSize = C.Sizeof $ C.Deref $ C.PMember expr "r0"
        let newSize = C.Infix C.Times cap elemSize
        let dataSize = C.Infix C.Times len elemSize
        appendElem $ C.Set pMem $ C.Call "GC_realloc" [pMem, newSize]

    void $ appendElem $ C.ExprStmt $ C.Increment $ C.PMember expr "len"


withFakeSwitch :: Generate a -> Generate a
withFakeSwitch f = do
    switchId <- appendElem $ C.Switch { switchBody = [], switchExpr = C.Int 0 }
    caseId <- newElement $ C.Case { caseExpr = C.Int 0, caseBody = [] }
    withCurID switchId $ append caseId
    withCurID caseId f
