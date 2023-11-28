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


callWithParams :: [Value] -> String -> [Value] -> Generate ()
callWithParams params name args = do
    void $ appendElem $ C.ExprStmt $ C.Call name (map ptrExpr params ++ map valExpr args)


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

        (t1, Type.Record [t2]) | t1 == t2 -> do
            set r $ Value typ $ C.Deref $ C.Member (valExpr val) "m0"

        (Type.Tuple (Type.Record ts1), Type.Record ts2) -> do
            check (length ts1 == length ts2) "cannot convert record of different length"
            forM_ (zip3 ts1 ts2 [0..]) $ \(t, t2, i) -> do
                m <- member i r
                set m =<< convert t (Value t2 $ C.Deref $ C.Member (valExpr val) ("m" ++ show i))

        _ -> error $ show (base, baseVal)

    return r


set :: Value -> Value -> Generate ()
set a b = do
    unless (typeof a == typeof b) (error "set: type mismatch")
    base <- baseTypeOf a
    void $ case base of
        _ | isSimple base               -> void $ appendElem $ C.Set (valExpr a) (valExpr b)
        Type.Tuple t -> do
            Type.Record ts <- baseTypeOf t
            forM_ (zip ts [0..]) $ \(t, i) -> do
                ma <- member i a
                mb <- member i b
                set ma mb

        Type.Record _ -> do
            ts <- getRecordTypes (typeof a)
            forM_ (zip ts [0..]) $ \(t, i) -> do
                let ma = Value t $ C.Deref $ C.Member (valExpr a) ("m" ++ show i)
                let mb = Value t $ C.Deref $ C.Member (valExpr b) ("m" ++ show i)
                set ma mb


        Type.ADT ts -> void $ appendElem $ C.Set (valExpr a) (valExpr b) -- TODO broken
            
--        Type.Table ts -> do
--            let cap = C.Member (valExpr a) "cap"
--            let len = C.Member (valExpr a) "len"
--            appendElem $ C.Set len (C.Member (valExpr b) "len")
--            appendElem $ C.Set cap (C.Member (valExpr b) "len")
--            forM_ (zip ts [0..]) $ \(t, i) -> do
--                let size = C.Sizeof $ C.Deref $ C.Member (valExpr a) ("r" ++ show i)
--                appendElem $ C.Set -- a.rn = GC_malloc(sizeof(*a.rn) * a.cap
--                    (C.Member (valExpr a) ("r" ++ show i))
--                    (C.Call "GC_malloc" [C.Infix C.Times size cap])
--                
--                for (Value I64 len) $ \idx -> set
--                    (Value t $ C.Subscript (C.Member (valExpr a) ("r" ++ show i)) $ valExpr idx)
--                    (Value t $ C.Subscript (C.Member (valExpr b) ("r" ++ show i)) $ valExpr idx)
--
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


-- {Person, bool}.0 -> Person = {string, i64}
-- {Person, bool}.1 -> {bool}
-- ()Person.0       -> string
-- ()Person.1       -> i64
member :: Int -> Value -> Generate Value
member index val = do
    base <- baseTypeOf val
    case base of
        Type.Record ts -> do
            Type.RecordTree ns <- getRecordTree (Type.Record ts)
            case ns !! index of
                RecordLeaf t i -> do
                    return $ Value t $ C.Deref $ C.Member (valExpr val) ("m" ++ show i)

                RecordTree _ -> do
                    leaves <- getRecordLeaves (ns !! index)
                    elems <- forM leaves $ \(t, i) -> do
                        return $ C.Member (valExpr val) ("m" ++ show i)
                    assign "record" $ Value (ts !! index) $ C.Initialiser elems

                x -> error (show x)

        Type.Tuple t -> do
            Type.Record ts <- baseTypeOf t
            Type.RecordTree ns <- getRecordTree t
            case ns !! index of
                RecordLeaf t i -> return $ Value (ts !! index) $
                    C.Member (valExpr val) ("m" ++ show i)

                RecordTree _ -> do
                    leaves <- getRecordLeaves (ns !! index)
                    assign "member" $ Value (ts !! index) $ C.Initialiser $
                        map (\(t, i) -> C.Address $ C.Member (valExpr val) ("m" ++ show i)) leaves

                x -> error (show x)

        Type.Table t -> do
            Type.Record ts <- baseTypeOf t
            Type.RecordTree ns <- getRecordTree t
            leaves <- getRecordLeaves (ns !! index)
            let elems  = map (\(t, i) -> C.Member (valExpr val) ("r" ++ show i)) leaves
            assign "member" $ Value (Table $ ts !! index) $ C.Initialiser $
                [ C.Member (valExpr val) "len"
                , C.Member (valExpr val) "cap"
                ] ++ elems

        Type.ADT ts   -> do
            unless (index >= 0 && index < length ts) (error "invalid ADT field index")
            return $ Value (ts !! index) $ C.Member (valExpr val) ("u" ++ show index)
        _ -> error (show base)


initialiser :: Type.Type -> [Value] -> Generate Value
initialiser typ [] = do
    base <- baseTypeOf typ
    case base of
--        Type.Tuple [] -> assign "zero" $ Value typ $ C.Initialiser []
        _             -> assign "zero" $ Value typ $ C.Initialiser [C.Int 0]
initialiser typ vals = do
    base <- baseTypeOf typ
    case base of
        Type.Tuple t -> do
            baseT <- baseTypeOf t
            case baseT of
                Type.Record _ -> do assign "tuple" $ Value typ $ C.Initialiser (map valExpr vals)
                _ -> error (show baseT)

        Type.Record _ -> do
            ts <- getRecordTypes typ
            unless (length ts == length vals) (error "initialiser length")
            assign "record" $ Value typ $ C.Initialiser $ map (C.Address . valExpr) vals

        Type.Range t -> do
            unless (map typeof vals == [t, t]) (error "initialiser types")
            assign "range" $ Value typ $ C.Initialiser (map valExpr vals)
        _ -> error (show base)


accessRecord :: Value -> (Maybe Value) -> Generate Value
accessRecord val marg = do
    base <- baseTypeOf val
    case base of
        _ | isSimple base -> do
            unless (isNothing marg) (error "no arg needed")
            assign "record" $ Value (Type.Record [typeof val]) $ C.Initialiser [C.Address $ valExpr val]

        Type.ADT ts -> do
            unless (isNothing marg) (error "not a table")
            assign "record" $ Value (Type.Record [typeof val]) $ C.Initialiser [C.Address $ valExpr val]

        Type.Record ts -> do
            unless (isNothing marg) (error "not a table")
            return val

        Table t -> do
            unless (isJust marg) (error "table access needs an integer argument")
            baseT <- baseTypeOf t
            case baseT of
                Type.Tuple _ -> accessRecord (Value t $ C.Subscript (C.Member (valExpr val) ("r" ++ show 0)) (valExpr $ fromJust marg)) Nothing
                _ | isSimple baseT -> do
                    elem <- return $ C.Address $ C.Subscript (C.Member (valExpr val) ("r" ++ show 0)) (valExpr $ fromJust marg)
                    assign "record" $ Value (Type.Record [t]) $ C.Initialiser [elem]
                ADT ts -> do
                    elem <- return $ C.Address $ C.Subscript (C.Member (valExpr val) ("r" ++ show 0)) (valExpr $ fromJust marg)
                    assign "record" $ Value (Type.Record [t]) $ C.Initialiser [elem]
                Type.Record _ -> do
                    ts <- getRecordTypes t
                    elems <- forM (zip ts [0..]) $ \(t, i) -> do
                        return $ C.Address $ C.Subscript (C.Member (valExpr val) ("r" ++ show i)) (valExpr $ fromJust marg)
                    assign "record" $ Value t $ C.Initialiser elems

                x -> error (show x)

        Type.Tuple t -> do
            unless (isNothing marg) (error "tuple access cannot have an argument")
            baseT <- baseTypeOf t
            case baseT of
                Type.Record _ -> do
                    ts <- getRecordTypes t
                    elems <- forM (zip ts [0..]) $ \(t, i) -> do
                        return $ C.Address $ C.Member (valExpr val) ("m" ++ show i)
                    assign "record" $ Value t $ C.Initialiser elems

                _ -> error (show baseT)

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
    Type.Record ts -> getTypedef "Record" =<< cTypeNoDef (Type.Record ts)
    Type.ADT ts    -> getTypedef "Adt"    =<< cTypeNoDef (Type.ADT ts)
    Type.Range t   -> getTypedef "Range"  =<< cTypeNoDef (Type.Range t)

    Type.TypeApply symbol args -> do
        (generics, typ) <- mapGet symbol =<< getTypeDefs
        getTypedef (Symbol.sym symbol) =<< cTypeNoDef =<< applyTypeArguments generics args typ

    _ -> error (show $ typeof a)

    where
        replaceVoid :: Type.Type -> Type.Type
        replaceVoid Void = I8
        replaceVoid t    = t

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
            Type.Tuple t -> do
                baseT <- baseTypeOf t
                case baseT of
                    Type.Record _ -> do
                        cts <- mapM cTypeOf =<< getRecordTypes t
                        return $ Cstruct $ zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] cts

            Type.Range t -> do
                ct <- cTypeOf t
                return $ Cstruct [ C.Param "min" ct, C.Param "max" ct ]

            Type.ADT ts -> do
                cts <- mapM cTypeOf (map replaceVoid ts)
                return $ Cstruct [C.Param "en" Cint64_t, C.Param "" $
                    Cunion $ map (\(ct, i) -> C.Param ("u" ++ show i) ct) (zip cts [0..])]

            Type.Table t -> do
                baseT <- baseTypeOf t
                cts <- mapM cTypeOf =<< case baseT of
                    Type.Record ts -> getRecordTypes t
                    t              -> return [t]
                let pts = zipWith (\ct i -> C.Param ("r" ++ show i) (Cpointer ct)) cts [0..]
                return $ Cstruct (C.Param "len" Cint64_t:C.Param "cap" Cint64_t:pts)

            Type.Record ts -> do
                cts <- mapM cTypeOf =<< getRecordTypes (Type.Record ts)
                return $ Cstruct $ zipWith (\ct i -> C.Param ("m" ++ show i) (Cpointer ct)) cts [0..]

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
        Type.Record _ -> getRecordTypes t
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
