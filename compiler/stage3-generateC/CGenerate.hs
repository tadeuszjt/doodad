{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CGenerate where

import Data.List
import Data.Char
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Monad
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
        , tableAppendFuncs :: Map.Map Type.Type String
        }

initGenerateState modName
    = GenerateState
        { moduleName = modName
        , tuples = Map.empty
        , supply = Map.empty
        , ctors  = Map.empty
        , typefuncs = Map.empty
        , symTab = SymTab.initSymTab
        , tableAppendFuncs = Map.empty
        }


class (MonadBuilder m, MonadFail m, MonadState GenerateState m, MonadIO m, MonadError Error m) => MonadGenerate m

newtype GenerateT m a = GenerateT { unGenerateT :: StateT GenerateState (StateT BuilderState (ExceptT Error m)) a }
    deriving (Functor, Applicative, Monad, MonadState GenerateState, MonadGenerate, MonadIO, MonadError Error)

instance Monad m => MonadBuilder (GenerateT m) where
    liftBuilderState (StateT s) = GenerateT $ lift $ StateT $ pure . runIdentity . s

instance MonadTrans GenerateT where
    lift = GenerateT . lift . lift . ExceptT . (fmap Right)

instance Monad m => MonadFail (GenerateT m) where
    fail s = throwError (ErrorStr s)

runGenerateT :: Monad m => GenerateState -> BuilderState -> GenerateT m a -> m (Either Error ((a, GenerateState), BuilderState))
runGenerateT generateState builderState generateT =
    runExceptT $ runStateT (runStateT (unGenerateT generateT) generateState) builderState


pushSymTab :: MonadGenerate m => m ()
pushSymTab = modify $ \s -> s { symTab = SymTab.push (symTab s) }


popSymTab :: MonadGenerate m => m ()
popSymTab = modify $ \s -> s { symTab = SymTab.pop (symTab s) }


define :: MonadGenerate m => String -> Value -> m ()
define str obj = withErrorPrefix "CGenerate: " $ do
    resm <- SymTab.lookup str () <$> gets symTab
    assert (isNothing resm) $ str ++ " already defined"
    modify $ \s -> s { symTab = SymTab.insert str () obj (symTab s) }

look :: MonadGenerate m => String -> m Value
look str = do
    resm <- SymTab.lookup str () <$> gets symTab
    when (isNothing resm) $ fail $ str ++ " isn't defined"
    return (fromJust resm)


fresh :: MonadGenerate m => String -> m String
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


assign :: MonadGenerate m => String -> Value -> m Value
assign suggestion val = do
    name <- fresh suggestion
    ctyp <- cTypeOf (typeof val)
    appendElem $ C.Assign ctyp name (valExpr val)
    return $ Value (typeof val) $ C.Ident name


if_ :: MonadGenerate m => Value -> m a -> m a
if_ cnd f = do
    base@(Type.Bool) <- baseTypeOf cnd
    id <- appendIf (valExpr cnd)
    withCurID id f


call :: MonadGenerate m => String -> [Value] -> m () 
call name args = do
    void $ appendElem $ C.ExprStmt $ C.Call name (map valExpr args)


callWithParams :: MonadGenerate m => [Value] -> String -> [Value] -> m ()
callWithParams params name args = do
    void $ appendElem $ C.ExprStmt $ C.Call name (map ptrExpr params ++ map valExpr args)


for :: MonadGenerate m => Value -> (Value -> m a) -> m a
for len f = do
    base@(I64) <- baseTypeOf len
    idx <- assign "idx" (i64 0)
    id <- appendElem $ C.For
        Nothing
        (Just $ C.Infix C.LT (valExpr idx) (valExpr len))
        (Just $ C.Increment $ valExpr idx)
        []
    withCurID id (f idx)


convert :: MonadGenerate m => Type.Type -> Value -> m Value
convert typ val = do
    base <- baseTypeOf typ
    baseVal <- baseTypeOf val
    r <- initialiser typ []
    case (base, baseVal) of
        _ | base == baseVal -> set r val

        (t1, Record [t2]) | t1 == t2 -> do
            set r $ Value typ $ C.Deref $ C.Member (valExpr val) "m0"

        (Type.Tuple (Record ts1), Record ts2) -> do
            assert (length ts1 == length ts2) "invalid type"
            forM_ (zip3 ts1 ts2 [0..]) $ \(t, t2, i) -> do
                m <- member i r
                set m =<< convert t (Value t2 $ C.Deref $ C.Member (valExpr val) ("m" ++ show i))

        _ -> error $ show (base, baseVal)

    return r




set :: MonadGenerate m => Value -> Value -> m ()
set a b = do
    assert (typeof a == typeof b) $ "set - types don't match: " ++ show (typeof a) ++ ", " ++ show (typeof b)
    base <- baseTypeOf a
    void $ case base of
        _ | isSimple base               -> void $ appendElem $ C.Set (valExpr a) (valExpr b)
--        Type.ADT fs                     -> void $ appendElem $ C.Set (valExpr a) (valExpr b)
--        Type.Tuple ts | all isSimple ts -> void $ appendElem $ C.Set (valExpr a) (valExpr b)
        Type.Tuple t -> do
            Record ts <- baseTypeOf t
            forM_ (zip ts [0..]) $ \(t, i) -> do
                ma <- member i a
                mb <- member i b
                set ma mb

        Type.Record ts -> do
            forM_ (zip ts [0..]) $ \(t, i) -> do
               void $ appendElem $ C.Set (C.Deref $ C.Member (valExpr a) ("m" ++ show i)) (C.Deref $ C.Member (valExpr b) ("m" ++ show i))
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


len :: MonadGenerate m => Value -> m Value
len val = do
    base <- baseTypeOf val
    case base of
        Table _      -> return $ Value I64 $ C.Member (valExpr val) "len"
        Type.String  -> return $ Value I64 $ C.Call "strlen" [valExpr val]
--        Type.Array n t -> return $ Value I64 $ C.Int (fromIntegral n)
        _ -> error (show base)


adtEnum :: MonadGenerate m => Value -> m Value
adtEnum obj = do
    base@(Type.ADT _) <- baseTypeOf obj
    return $ Value I64 $ C.Member (valExpr obj) "en"


member :: MonadGenerate m => Int -> Value -> m Value
member index val = do
    base <- baseTypeOf val
    case base of
        Type.Record ts -> do
            typeDefs <- gets typefuncs
            let Type.RecordTree ns = getRecordTree typeDefs (Type.Record ts)
            case ns !! index of
                --x -> fail (show val)
                Type.RecordLeaf t i -> assign "deref" $ Value t $ -- TODO i don't want to do this
                    C.Deref $ C.Member (valExpr val) ("m" ++ show i)
--                Type.RecordLeaf t i -> return $ Value (Type.Record [t]) $
--                    C.Member (valExpr val) ("m" ++ show i)
                x -> error (show x)
        Type.Tuple t -> do
            typeDefs <- gets typefuncs
            Type.Record ts <- baseTypeOf t
            let Type.RecordTree ns = getRecordTree typeDefs t
            case ns !! index of
                RecordLeaf t i -> return $ Value (ts !! index) $ C.Member (valExpr val) ("m" ++ show i)
                x -> error (show x)

        Type.ADT ts   -> do
            assert (index >= 0 && index < length ts) "invalid ADT field index"
            return $ Value (ts !! index) $ C.Member (valExpr val) ("u" ++ show index)
        _ -> error (show base)


initialiser :: MonadGenerate m => Type.Type -> [Value] -> m Value
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
                Record _ -> do assign "tuple" $ Value typ $ C.Initialiser (map valExpr vals)
                _ -> error (show baseT)

        Type.Record _ -> do
            typeDefs <- gets typefuncs
            let ts = getRecordTypes typeDefs typ
            assert (length ts == length vals) "initialiser length"
            assign "record" $ Value typ $ C.Initialiser $ map (C.Address . valExpr) vals

        Type.Range t -> do
            assert (map typeof vals == [t, t]) "initialiser types"
            assign "range" $ Value typ $ C.Initialiser (map valExpr vals)
        _ -> error (show base)


accessRecord :: MonadGenerate m => Value -> (Maybe Value) -> m Value
accessRecord val marg = do
    base <- baseTypeOf val
    case base of
        _ | isSimple base -> do
            assert (isNothing marg) "no arg needed"
            assign "record" $ Value (Record [typeof val]) $ C.Initialiser [C.Address $ valExpr val]

        Table t -> do
            assert (isJust marg) "table access needs an integer argument"
            baseT <- baseTypeOf t
            case baseT of
                Type.Tuple _ -> accessRecord (Value t $ C.Subscript (C.Member (valExpr val) ("r" ++ show 0)) (valExpr $ fromJust marg)) Nothing
                _ | isSimple baseT -> do
                    elem <- return $ C.Address $ C.Subscript (C.Member (valExpr val) ("r" ++ show 0)) (valExpr $ fromJust marg)
                    assign "record" $ Value (Record [t]) $ C.Initialiser [elem]
                Record _ -> do
                    typeDefs <- gets typefuncs
                    let ts = getRecordTypes typeDefs t
                    elems <- forM (zip ts [0..]) $ \(t, i) -> do
                        return $ C.Address $ C.Subscript (C.Member (valExpr val) ("r" ++ show i)) (valExpr $ fromJust marg)
                    assign "record" $ Value t $ C.Initialiser elems
                x -> error (show x)

        Type.Tuple t -> do
            assert (isNothing marg) "tuple access cannot have an argument"
            baseT <- baseTypeOf t
            case baseT of
                Record _ -> do
                    typeDefs <- gets typefuncs
                    let ts = getRecordTypes typeDefs t
                    elems <- forM (zip ts [0..]) $ \(t, i) -> do
                        return $ C.Address $ C.Member (valExpr val) ("m" ++ show i)
                    assign "record" $ Value t $ C.Initialiser elems

                _ -> error (show baseT)
            

        x -> error (show x)

        _ -> error (show base)



baseTypeOf :: (MonadGenerate m, Typeof a) => a -> m Type.Type
baseTypeOf a = case typeof a of
    Type.TypeApply symbol ts -> do
        resm <- Map.lookup symbol <$> gets typefuncs
        case resm of
            Nothing                -> fail $ "baseTypeOf: " ++ show (typeof a)
            Just (argSymbols, typ) -> do
                assert (length argSymbols == length ts) "invalid number of type arguments"
                baseTypeOf $ applyTypeArguments argSymbols ts typ

--            Just (symbols, typ) -> do
--                assert (length ts == length symbols) $ "Invalid type function arguments: " ++ show ts
--                baseTypeOf $ applyTypeArguments (Map.fromList $ zip symbols ts) typ
        
    _ -> return (typeof a)
    _ -> error (show $ typeof a)


cParamOf :: MonadGenerate m => S.Param -> m C.Param
cParamOf param = do
    ctype <- cTypeOf (paramType param)
    return $ C.Param { C.cName = show (paramName param), C.cType = ctype }


cTypeOf :: (MonadGenerate m, Typeof a) => a -> m C.Type
cTypeOf a = case typeof a of
    I64 -> return $ Cint64_t
    I32 -> return $ Cint32_t
    I8 ->  return $ Cint8_t
    U8 ->  return $ Cuint8_t
    F64 -> return $ Cdouble
    F32 -> return $ Cfloat
    Void -> return Cvoid
    Type.Bool -> return $ Cbool
    Type.Char -> return $ Cchar
    Type.String -> return $ Cpointer Cchar
    Type.TypeApply symbol argTypes -> do
        (argSymbols, typ) <- mapGet symbol =<< gets typefuncs
        assert (length argSymbols == length argTypes) "invalid number of type arguments"
        getTypedef (Symbol.sym symbol) =<< cTypeOf (applyTypeArguments argSymbols argTypes typ)
    Type.Tuple t -> do
        baseT <- baseTypeOf t
        case baseT of
            Record _ -> do
                typeDefs <- gets typefuncs
                cts <- mapM cTypeOf (getRecordTypes typeDefs t)
                getTypedef "tuple" $ Cstruct $ zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] cts
            t -> cTypeOf t

    Type.Table t -> do
        base <- baseTypeOf t
        typeDefs <- gets typefuncs
        ts <- case base of
            Record ts -> return (getRecordTypes typeDefs t)
            t         -> return [t]
        cts <- mapM cTypeOf ts
        let pts = zipWith (\ct i -> C.Param ("r" ++ show i) (Cpointer ct)) cts [0..]
        getTypedef "table" $ Cstruct (C.Param "len" Cint64_t:C.Param "cap" Cint64_t:pts)

    Type.Record ts -> do
        typeDefs <- gets typefuncs
        cts <- mapM cTypeOf (getRecordTypes typeDefs $ Type.Record ts)
        getTypedef "record" $ Cstruct $ zipWith (\ct i -> C.Param ("m" ++ show i) (Cpointer ct)) cts [0..]
--    Type.Array n t -> do
--        arr <- Carray n <$> cTypeOf t
--        getTypedef "array" $ Cstruct [C.Param "arr" arr]
    Type.Range t -> do
        ct <- cTypeOf t
        getTypedef "range" $ Cstruct [C.Param "min" ct, C.Param "max" ct]
    Type.ADT ts -> do
        cts <- mapM cTypeOf (map replaceVoid ts)
        getTypedef "adt" $ Cstruct [C.Param "en" Cint64_t, C.Param "" $
            Cunion $ map (\(ct, i) -> C.Param ("u" ++ show i) ct) (zip cts [0..])]


    _ -> error (show $ typeof a)

    where
        replaceVoid :: Type.Type -> Type.Type
        replaceVoid Void = I8
        replaceVoid t    = t



getTypedef :: MonadGenerate m => String -> C.Type -> m C.Type
getTypedef suggestion typ = do
    sm <- Map.lookup typ <$> gets tuples
    case sm of
        Just s -> return $ Ctypedef s
        Nothing -> do
            name <- fresh suggestion
            newTypedef typ name
            modify $ \s -> s { tuples = Map.insert typ name (tuples s) }
            return $ Ctypedef name


getTableAppendFunc :: MonadGenerate m => Type.Type -> m String
getTableAppendFunc typ = do
    base@(Table t) <- baseTypeOf typ
    baseT <- baseTypeOf t
    ts <- case baseT of
        Record ts -> return ts
        t         -> return [t]
    fm <- Map.lookup typ <$> gets tableAppendFuncs
    case fm of
        Just s -> return s
        Nothing -> do -- append multiple tables
            modName <- gets moduleName
            funcName <- fresh $ modName ++ "_table_append"
            aParam <- C.Param "a" . Cpointer <$> cTypeOf typ
            --bParam <- C.Param "b" . Cpointer <$> cTypeOf typ

            -- create new function
            funcId <- newFunction Cvoid funcName [aParam] --bParam]
            withCurID globalID $ append funcId
            withCurID funcId $ do
                let a = C.Ident "a"
                --let b = C.Ident "b"
                let len = C.PMember a "len"
                --let len2 = C.PMember b "len" 
                let newLen = (C.Infix C.Plus len $ C.Int 1)
                let cap = C.PMember a "cap"
                
                -- realloc if needed
                if_ (Value Type.Bool $ C.Infix C.GTEq newLen cap) $ do
                    appendElem $ C.Set cap (C.Infix C.Times newLen (C.Int 2))
                    forM_ (zip ts [0..]) $ \(t, row) -> do
                        appendElem $ C.Assign
                            (Cpointer Cvoid)
                            ("mem" ++ show row)
                            (C.Call "GC_malloc" [C.Infix C.Times cap (C.Sizeof $ C.Deref $ C.PMember a $ "r" ++ show row)])
                    forM_ (zip ts [0..]) $ \(t, row) -> do
                        appendElem $ C.ExprStmt $ C.Call "memcpy"
                            [ C.Ident ("mem" ++ show row)
                            , C.PMember a ("r" ++ show row)
                            , C.Infix C.Times len (C.Sizeof $ C.Deref $ C.PMember a $ "r" ++ show row)]
                    forM_ (zip ts [0..]) $ \(t, row) -> do
                        appendElem $ C.Set (C.PMember a ("r" ++ show row)) (C.Ident $ "mem" ++ show row)

                for (Value I64 $ C.Int 1) $ \idx -> do
--                    forM_ (zip ts [0..]) $ \(t, row) -> do
--                        set
--                            (Value t $ C.Subscript (C.PMember a $ "r" ++ show row) (C.PMember a "len"))
--                            (Value t $ C.Initialiser [])
--                            --(Value t $ C.Subscript (C.PMember b $ "r" ++ show row) (valExpr idx))
                    void $ appendElem $ C.ExprStmt $ C.Increment $ C.PMember a "len"
                    
            modify $ \s -> s { tableAppendFuncs = Map.insert typ funcName (tableAppendFuncs s) }
            return funcName
