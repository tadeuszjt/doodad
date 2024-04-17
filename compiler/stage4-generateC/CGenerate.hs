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
    | Const S.Expr
    deriving (Show, Eq)

instance Typeof Value where
    typeof (Value t _) = t


ptrExpr :: Value -> C.Expression
ptrExpr obj = case obj of
    Value t (C.Deref e) -> e
    Value t e   -> C.Address e


data GenerateState
    = GenerateState
        { moduleName :: String
        , tuples :: Map.Map C.Type String
        , supply :: Map.Map String Int
        , ctors  :: Map.Map Symbol (Symbol, Int)
        , typefuncs :: Map.Map Symbol ([Symbol], Type.Type)
        , symTab :: SymTab.SymTab String () Value
        }

initGenerateState modName
    = GenerateState
        { moduleName = modName
        , tuples = Map.empty
        , supply = Map.empty
        , ctors  = Map.empty
        , typefuncs = Map.empty
        , symTab = SymTab.initSymTab
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


assign :: String -> Value -> Generate Value
assign suggestion val = do
    name <- fresh suggestion
    ctyp <- cTypeOf (typeof val)
    appendElem $ C.Assign ctyp name (valExpr val)
    return $ Value (typeof val) $ C.Ident name


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
    baseVal <- baseTypeOf val
    r <- initialiser typ []
    case (base, baseVal) of
        _ | base == baseVal -> do
            let Value _ expr = val
            set r (Value typ expr)

        (F32, f64)       -> set r $ Value typ $ C.Cast Cfloat (valExpr val)
        (F32, I64)       -> set r $ Value typ $ C.Cast Cfloat (valExpr val)
        (I64, Type.Char) -> set r $ Value typ $ C.Cast Cint64_t (valExpr val)

        _ -> error $ show (base, baseVal)

    return r


set :: Value -> Value -> Generate ()
set a b = do
    unless (typeof a == typeof b) (error "set: type mismatch")
    base <- baseTypeOf a
    void $ case base of
        _ | isSimple base               -> void $ appendElem $ C.Set (valExpr a) (valExpr b)
        Type.Tuple ts -> do
            forM_ (zip ts [0..]) $ \(t, i) -> do
                let va = Value t $ C.Member (valExpr a) ("m" ++ show i)
                let vb = Value t $ C.Member (valExpr b) ("m" ++ show i)
                set va vb

        Type.ADT ts -> void $ appendElem $ C.Set (valExpr a) (valExpr b) -- TODO broken
            
        Type.Table t -> do
            error ""
--            let cap = C.Member (valExpr a) "cap"
--            let len = C.Member (valExpr a) "len"
--            appendElem $ C.Set len (C.Member (valExpr b) "len")
--            appendElem $ C.Set cap (C.Member (valExpr b) "len")
--
--            ts <- getRecordTypes t
--            forM_ (zip ts [0..]) $ \(t, i) -> do
--                let size = C.Sizeof $ C.Deref $ C.Member (valExpr a) ("r" ++ show i)
--                appendElem $ C.Set -- a.rn = GC_malloc(sizeof(*a.rn) * a.cap
--                    (C.Member (valExpr a) ("r" ++ show i))
--                    (C.Call "GC_malloc" [C.Infix C.Times size cap])
--                
--                for (Value I64 len) $ \idx -> set
--                    (Value t $ C.Subscript (C.Member (valExpr a) ("r" ++ show i)) $ valExpr idx)
--                    (Value t $ C.Subscript (C.Member (valExpr b) ("r" ++ show i)) $ valExpr idx)

--        Type.Array n t -> do
--            for (i64 n) $ \idx -> do
--                sa <- subscript a idx
--                sb <- subscript b idx
--                set sa sb


        _ -> error (show base)


len :: Value -> Generate Value
len val = do
    base <- baseTypeOf val
    case base of
        Table _      -> return $ Value I64 $ C.Member (valExpr val) "len"
        Type.String  -> return $ Value I64 $ C.Call "strlen" [valExpr val]
--        Type.Array n t -> return $ Value I64 $ C.Int (fromIntegral n)
        _ -> error (show base)


adtEnum :: Value -> Generate Value
adtEnum obj = do
    base@(Type.ADT _) <- baseTypeOf obj
    return $ Value I64 $ C.Member (valExpr obj) "en"


member :: Int -> Value -> Generate Value
member index val = do
    base <- baseTypeOf val
    case base of
        Type.Tuple ts -> do
            unless (index >= 0 && index < length ts) (error "invalid member index")
            return $ Value (ts !! index) $ C.Member (valExpr val) ("m" ++ show index)

        Type.Table t -> do
            error ""
--            Type.Record ts <- baseTypeOf t
--            Type.RecordTree ns <- getRecordTree t
--            leaves <- getRecordLeaves (ns !! index)
--            let elems  = map (\(t, i) -> C.Member (valExpr val) ("r" ++ show i)) leaves
--            assign "member" $ Value (Table $ ts !! index) $ C.Initialiser $
--                [ C.Member (valExpr val) "len"
--                , C.Member (valExpr val) "cap"
--                ] ++ elems

        Type.ADT ts   -> do
            unless (index >= 0 && index < length ts) (error "invalid ADT field index")
            return $ Value (ts !! index) $ C.Member (valExpr val) ("u" ++ show index)
        _ -> error (show base)


initialiser :: Type.Type -> [Value] -> Generate Value
initialiser typ [] = do
    base <- baseTypeOf typ
    case base of
        Type.Reference _ -> fail "cannot initialize empty reference"
        _           -> assign "zero" $ Value typ $ C.Initialiser [C.Int 0]
initialiser typ vals = do
    base <- baseTypeOf typ
    case base of
        Type.Reference t -> fail "cannot initialise reference"
        Type.Tuple ts -> do
            unless (length vals == length ts) (fail "invalid tuple initialiser")
            assign "zero" $ Value typ $ C.Initialiser (map valExpr vals)

        _ -> error (show base)


builtinTableGet :: Value -> Value -> Generate Value
builtinTableGet val idx = do
    Table t <- baseTypeOf val
    baseT <- baseTypeOf t
    case baseT of
        _ | isSimple baseT -> return $ Value t $ C.Subscript (C.Member (valExpr val) "r0") (valExpr idx)
        Type.Tuple _ -> error ""

            
        ADT ts -> error ""
        Type.Table _ -> error ""
        x -> error (show x)


builtinTableAt :: Value -> Value -> Generate Value
builtinTableAt val idx = do
    Type.Reference tabTyp <- baseTypeOf val
    I64 <- baseTypeOf idx
    Table t <- baseTypeOf tabTyp
    baseT <- baseTypeOf t
    case baseT of
        x | isSimple x -> return $ Value (Type.Reference t) $ C.Address $ C.Subscript
            (C.Member (C.Deref $ valExpr val) "r0")
            (valExpr idx)

        x -> error (show x)



cParamOf :: S.Param -> Generate C.Param
cParamOf param = do
    ctype <- cTypeOf (paramType param)
    return $ C.Param { C.cName = show (paramName param), C.cType = ctype }


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
    Type.String    -> return (Cpointer Cchar)
    Type.Tuple t   -> getTypedef "Tuple"  =<< cTypeNoDef (Type.Tuple t)
    Type.Table t   -> getTypedef "Table"  =<< cTypeNoDef (Type.Table t)
    Type.ADT ts    -> getTypedef "Adt"    =<< cTypeNoDef (Type.ADT ts)

    Type.Reference t -> do
        base <- baseTypeOf t
        case base of
            _ | isSimple base -> Cpointer <$> cTypeOf t
            Table _           -> Cpointer <$> cTypeOf t
            Type.Tuple _      -> getTypedef "Reference" =<< cTypeNoDef (Type.Reference t)


            x -> error (show x)

    Type.TypeApply symbol args -> do
        (generics, typ) <- mapGet symbol =<< getTypeDefs
        getTypedef (Symbol.sym symbol) =<< cTypeNoDef =<< applyTypeArguments generics args typ

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
            Type.String -> return (Cpointer Cchar)
            Type.Tuple ts -> do
                cts <- mapM cTypeOf ts
                return $ Cstruct $ map (\(ct, i) -> C.Param ("m" ++ show i) ct) (zip cts [0..])

            Type.ADT ts -> do
                cts <- mapM cTypeOf ts
                return $ Cstruct [C.Param "en" Cint64_t, C.Param "" $
                    Cunion $ map (\(ct, i) -> C.Param ("u" ++ show i) ct) (zip cts [0..])]

            Type.Table t -> do
                baseT <- baseTypeOf t
                cts <- mapM cTypeOf =<< case baseT of
                    t              -> return [t]
                let pts = zipWith (\ct i -> C.Param ("r" ++ show i) (Cpointer ct)) cts [0..]
                return $ Cstruct (C.Param "len" Cint64_t:C.Param "cap" Cint64_t:pts)

            Type.Reference t -> do
                baseT <- baseTypeOf t
                case baseT of
                    _ | isSimple baseT -> Cpointer <$> cTypeOf t
                    x -> error (show x)

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
tableAppend val = do
    Table t <- baseTypeOf val
    baseT <- baseTypeOf t
    ts <- case baseT of
        _             -> return [t]

    let len    = C.Member (valExpr val) "len"
    let newLen = (C.Infix C.Plus len $ C.Int 1)
    let cap    = C.Member (valExpr val) "cap"
    
    -- realloc if needed
    if_ (Value Type.Bool $ C.Infix C.GTEq len cap) $ do
        appendElem $ C.Set cap (C.Infix C.Times newLen (C.Int 2))

        forM_ (zip ts [0..]) $ \(t, row) -> do
            let pMem = C.Member (valExpr val) ("r" ++ show row)
            let elemSize = C.Sizeof $ C.Deref $ C.Member (valExpr val) $ "r" ++ show row
            let newSize = C.Infix C.Times cap elemSize
            let dataSize = C.Infix C.Times len elemSize
            appendElem $ C.Set pMem $ C.Call "GC_realloc" [pMem, newSize]

    void $ appendElem $ C.ExprStmt $ C.Increment $ C.Member (valExpr val) "len"


withFakeSwitch :: Generate a -> Generate a
withFakeSwitch f = do
    switchId <- appendElem $ C.Switch { switchBody = [], switchExpr = C.Int 0 }
    caseId <- newElement $ C.Case { caseExpr = C.Int 0, caseBody = [] }
    withCurID switchId $ append caseId
    withCurID caseId f
