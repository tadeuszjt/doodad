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
import States
import CBuilder as C
import CAst as C
import Control.Monad.State
import Type
import AST as S
import Error

data Value
    = Value { valType :: Type.Type, valExpr :: C.Expression }
    deriving (Show, Eq)

instance Typeof Value where
    typeof (Value t _) = t


ptrExpr :: Value -> C.Expression
ptrExpr obj = case obj of
    Value t (C.Deref e) -> e
    Value t e   -> C.Address e


data GenerateState
    = GenerateState
        { tuples :: Map.Map C.Type String
        , supply :: Map.Map String Int
        , ctors  :: Map.Map Symbol (Type.Type, Int)
        , typedefs :: Map.Map Symbol Type.Type
        , symTab :: Map.Map String Value
        , tableAppendFuncs :: Map.Map Type.Type String
        }

initGenerateState
    = GenerateState
        { tuples = Map.empty
        , supply = Map.empty
        , ctors  = Map.empty
        , typedefs = Map.empty
        , symTab = Map.empty
        , tableAppendFuncs = Map.empty
        }


class (MonadBuilder m, MonadFail m, MonadState GenerateState m, MonadIO m) => MonadGenerate m

newtype GenerateT m a = GenerateT { unGenerateT :: StateT GenerateState (StateT BuilderState m) a }
    deriving (Functor, Applicative, Monad, MonadState GenerateState, MonadGenerate, MonadIO)

instance Monad m => MonadBuilder (GenerateT m) where
    liftBuilderState (StateT s) = GenerateT $ lift $ StateT $ pure . runIdentity . s

instance MonadTrans GenerateT where
    lift = GenerateT . lift . lift 

instance Monad m => MonadFail (GenerateT m) where
    fail = error

runGenerateT :: Monad m => GenerateState -> BuilderState -> GenerateT m a -> m ((a, GenerateState), BuilderState)
runGenerateT generateState builderState generateT =
    runStateT (runStateT (unGenerateT generateT) generateState) builderState


define :: MonadGenerate m => String -> Value -> m ()
define str obj = do
    isDefined <- Map.member str <$> gets symTab
    assert (not isDefined) $ str ++ " already defined"
    modify $ \s -> s { symTab = Map.insert str obj (symTab s) }

look :: MonadGenerate m => String -> m Value
look str = (Map.! str) <$> gets symTab


freshName :: MonadGenerate m => String -> m String
freshName suggestion = do
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


assignI64 :: MonadGenerate m => String -> Int -> m Value
assignI64 suggestion n = do
    name <- freshName suggestion
    appendElem $ C.Assign Cint64_t name (C.Int $ fromIntegral n)
    return $ Value I64 $ C.Ident name

assignBool :: MonadGenerate m => String -> Bool -> m Value
assignBool suggestion b = do
    name <- freshName suggestion
    appendElem $ C.Assign Cbool name (C.Bool b)
    return $ Value Type.Bool $ C.Ident name

assign :: MonadGenerate m => String -> Value -> m Value
assign suggestion val = do
    name <- freshName suggestion
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



set :: MonadGenerate m => Value -> Value -> m ()
set a b = do
    assert (typeof a == typeof b) "set: types don't match"
    base <- baseTypeOf a
    void $ case base of
        Type.Bool -> appendElem $ C.Set (valExpr a) (valExpr b)
        Type.ADT fs -> appendElem $ C.Set (valExpr a) (valExpr b)
        Type.String -> appendElem $ C.Set (valExpr a) (valExpr b)
        Type.Char   -> appendElem $ C.Set (valExpr a) (valExpr b)
        Type.I64   -> appendElem $ C.Set (valExpr a) (valExpr b)
        Type.F32   -> appendElem $ C.Set (valExpr a) (valExpr b)
        Type.F64   -> appendElem $ C.Set (valExpr a) (valExpr b)
        _ -> error (show base)


len :: MonadGenerate m => Value -> m Value
len val = do
    base <- baseTypeOf val
    case base of
        Table ts    -> return $ Value I64 $ C.Member (valExpr val) "len"
        Type.String -> return $ Value I64 $ C.Call "strlen" [valExpr val]
        _ -> error (show base)


adtEnum :: MonadGenerate m => Value -> m Value
adtEnum obj = do
    base@(Type.ADT fs) <- baseTypeOf obj
    return $ Value I64 $ C.Member (valExpr obj) "en"


member :: MonadGenerate m => Int -> Value -> m Value
member i val = do
    base <- baseTypeOf val
    case base of
        Type.Table ts -> assign "row" $ Value (Type.Table [ts !! i]) $ C.Initialiser
            [ C.Member (valExpr val) "len"
            , C.Member (valExpr val) "cap"
            , C.Member (valExpr val) ("r" ++ show i)]
        Type.Tuple ts -> return $ Value (ts !! i) $ C.Member (valExpr val) ("m" ++ show i)
        Type.ADT fs   -> case fs !! i of
            FieldNull -> fail "no val for null field"
            FieldType t -> return $ Value t $ C.Member (valExpr val) ("u" ++ show i)
            FieldCtor [] -> fail "no val for empty ctor"
            FieldCtor [t] -> return $ Value t $ C.Member (valExpr val) ("u" ++ show i)
            FieldCtor ts -> return $ Value (Type.Tuple ts) $ C.Member (valExpr val) ("u" ++ show i)


subscript :: MonadGenerate m => Value -> Value -> m Value
subscript val idx = do
    base <- baseTypeOf val
    baseIdx <- baseTypeOf idx
    assert (isInt baseIdx) "idx type isn't integer"
    case base of
        Type.Array n t -> return $ Value t $ C.Subscript (C.Member (valExpr val) "arr") (valExpr idx)
        Type.String -> return $ Value Type.Char $ C.Subscript (valExpr val) (valExpr idx)
        Type.Table [t] -> return $ Value t $ C.Subscript (C.Member (valExpr val) "r0") (valExpr idx)
        Type.Table ts -> do
            elems <- forM (zip ts [0..]) $ \(t, i) -> do
                return $ C.Subscript (C.Member (valExpr val) ("r" ++ show i)) (valExpr idx)
            assign "subscr" $ Value (Type.Tuple ts) $ C.Initialiser elems

            
baseTypeOf :: (MonadGenerate m, Typeof a) => a -> m Type.Type
baseTypeOf a = case typeof a of
    Type.Typedef s -> baseTypeOf . (Map.! s) =<< gets typedefs
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
    Type.Typedef s -> return $ Ctypedef (show s)
    Type.String -> return $ Cpointer Cchar
    Type.Array n t -> do
        arr <- Carray n <$> cTypeOf t
        getTypedef "array" $ Cstruct [C.Param "arr" arr]
    Type.Tuple ts -> do
        cts <- mapM cTypeOf ts
        getTypedef "tuple" $ Cstruct $ zipWith (\a b -> C.Param ("m" ++ show a) b) [0..] cts
    Type.Range t -> do
        ct <- cTypeOf t
        getTypedef "range" $ Cstruct [C.Param "min" ct, C.Param "max" ct]
    Type.ADT fs -> do
        cts <- mapM cTypeOf (map fieldType fs)
        getTypedef "adt" $ Cstruct [C.Param "en" Cint64_t, C.Param "" $
            Cunion $ map (\(ct, i) -> C.Param ("u" ++ show i) ct) (zip cts [0..])]
    Type.Table ts -> do
        cts <- mapM cTypeOf ts
        let pts = zipWith (\ct i -> C.Param ("r" ++ show i) (Cpointer ct)) cts [0..]
        getTypedef "table" $ Cstruct (C.Param "len" Cint64_t:C.Param "cap" Cint64_t:pts)

    _ -> error (show $ typeof a)
    where
        fieldType f = case f of
            FieldNull -> I8
            FieldType t -> t
            FieldCtor [t] -> t
            FieldCtor ts -> Type.Tuple ts

getTypedef :: MonadGenerate m => String -> C.Type -> m C.Type
getTypedef suggestion typ = do
    sm <- Map.lookup typ <$> gets tuples
    case sm of
        Just s -> return $ Ctypedef s
        Nothing -> do
            name <- freshName suggestion
            newTypedef typ name
            modify $ \s -> s { tuples = Map.insert typ name (tuples s) }
            return $ Ctypedef name


getTableAppendFunc :: MonadGenerate m => Type.Type -> m String
getTableAppendFunc typ = do
    base@(Table ts) <- baseTypeOf typ
    fm <- Map.lookup typ <$> gets tableAppendFuncs
    case fm of
        Just s -> return s
        Nothing -> do -- append multiple tables
            funcName <- freshName "doodad_table_append"
            aParam <- C.Param "a" . Cpointer <$> cTypeOf typ
            bParam <- C.Param "b" . Cpointer <$> cTypeOf typ

            -- create new function
            funcId <- newFunction Cvoid funcName [aParam, bParam]
            withCurID globalID $ append funcId
            withCurID funcId $ do
                let a = C.Ident "a"
                let b = C.Ident "b"
                let len = C.PMember a "len"
                let len2 = C.PMember b "len" 
                let newLen = (C.Infix C.Plus len len2)
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

                -- append elements TODO use deep copy
                idx <- assign "idx" (i64 0)
                forId <- appendElem $ C.For
                    Nothing
                    (Just $ C.Infix C.LT (valExpr idx) len2)
                    (Just $ C.Increment $ valExpr idx)
                    []
                withCurID forId $ do
                    forM_ (zip ts [0..]) $ \(t, row) -> do
                        appendElem $ C.Set
                            (C.Subscript (C.PMember a $ "r" ++ show row) (C.PMember a "len"))
                            (C.Subscript (C.PMember b $ "r" ++ show row) (valExpr idx))
                    void $ appendElem $ C.ExprStmt $ C.Increment $ C.PMember a "len"
                    
            modify $ \s -> s { tableAppendFuncs = Map.insert typ funcName (tableAppendFuncs s) }
            return funcName