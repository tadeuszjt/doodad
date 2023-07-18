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

data Object
    = Pointer Type.Type C.Expression
    | Value   Type.Type C.Expression
    deriving (Show, Eq)

instance Typeof Object where
    typeof (Value t _) = t
    typeof (Pointer t _) = t

valueExpr :: Object -> C.Expression
valueExpr object = case object of
    Pointer t e -> C.Deref (e)
    Value t e -> e

pointerExpr :: Object -> C.Expression
pointerExpr obj = case obj of
    Pointer t e -> e
    Value t e   -> C.Address e


data GenerateState
    = GenerateState
        { tuples :: Map.Map C.Type String
        , supply :: Map.Map String Int
        , ctors  :: Map.Map Symbol (Type.Type, Int)
        , typedefs :: Map.Map Symbol Type.Type
        , symTab :: Map.Map String Object
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


class (MonadBuilder m, MonadFail m, MonadState GenerateState m) => MonadGenerate m

newtype GenerateT m a = GenerateT { unGenerateT :: StateT GenerateState (StateT BuilderState m) a }
    deriving (Functor, Applicative, Monad, MonadState GenerateState, MonadGenerate)

instance Monad m => MonadBuilder (GenerateT m) where
    liftBuilderState (StateT s) = GenerateT $ lift $ StateT $ pure . runIdentity . s

instance MonadTrans GenerateT where
    lift = GenerateT . lift . lift 

instance Monad m => MonadFail (GenerateT m) where
    fail = error

runGenerateT :: Monad m => GenerateState -> BuilderState -> GenerateT m a -> m ((a, GenerateState), BuilderState)
runGenerateT generateState builderState generateT =
    runStateT (runStateT (unGenerateT generateT) generateState) builderState


define :: MonadGenerate m => String -> Object -> m ()
define str obj = do
    isDefined <- Map.member str <$> gets symTab
    assert (not isDefined) $ str ++ " already defined"
    modify $ \s -> s { symTab = Map.insert str obj (symTab s) }

look :: MonadGenerate m => String -> m Object
look str = (Map.! str) <$> gets symTab

freshName :: MonadGenerate m => String -> m String
freshName suggestion = do
    nm <- Map.lookup suggestion <$> gets supply
    let n = maybe 0 id nm
    modify $ \s -> s { supply = Map.insert suggestion (n + 1) (supply s) }
    return $ suggestion ++ show n

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
    base@(Table _) <- baseTypeOf typ
    fm <- Map.lookup typ <$> gets tableAppendFuncs
    case fm of
        Just s -> return s
        Nothing -> do
            funcName <- freshName "doodad_table_append"
            tableName <- freshName "table"
            elemName <- freshName "elem"
            ctyp <- cTypeOf typ

            celemTyp <- case base of
                Table [t] -> cTypeOf t

            funcId <- newFunction Cvoid funcName
                [ C.Param tableName $ Cpointer ctyp
                , C.Param elemName $ Cpointer celemTyp
                ]
            withCurID globalID $ append funcId

            withCurID funcId $ do
                let tableIdent = C.Ident tableName
                ifId <- appendIf $ C.Infix C.GTEq
                    (C.PMember (tableIdent) "len")
                    (C.PMember (tableIdent) "cap")
                withCurID ifId $ do
                    ifCap0Id <- appendIf $ C.Infix C.EqEq
                        (C.PMember tableIdent "cap")
                        (C.Int 0)
                    withCurID ifCap0Id $ do
                        appendElem $ C.Set (C.PMember tableIdent "cap") (C.Int 8)
                    elseCap0Id <- appendElem $ C.Else []
                    withCurID elseCap0Id $ do
                        appendElem $ C.Set (C.PMember tableIdent "cap") $
                            C.Infix C.Times (C.PMember tableIdent "cap") (C.Int 2)
                    memName <- freshName "mem"
                    appendElem $ C.Assign (Cpointer Cvoid) memName $
                        C.Call "GC_malloc"
                            [ C.Infix C.Times (C.PMember tableIdent "cap") $
                                C.Sizeof (C.Deref $ C.PMember tableIdent "r0")
                            ]
                    appendElem $ C.ExprStmt $ C.Call "memcpy"
                        [ C.Ident memName
                        , C.PMember tableIdent "r0"
                        , C.Infix C.Times (C.PMember tableIdent "len")
                            (C.Sizeof (C.Deref $ C.PMember tableIdent "r0"))
                        ]
                    appendElem $ C.Set (C.PMember tableIdent "r0") (C.Ident memName)
                    return ()

                -- TODO deep copy
                appendElem $ C.Set
                    (C.Subscript
                        (C.PMember tableIdent "r0")
                        (C.Increment $ C.PMember (C.Ident tableName) "len"))
                    (C.Deref $ C.Ident elemName)

                return ()

            modify $ \s -> s { tableAppendFuncs = Map.insert typ funcName (tableAppendFuncs s) }
            return funcName

            
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
        

getSymbolsOrderedByDependencies :: Monad m => Map.Map Symbol Type.Type -> m [Symbol]
getSymbolsOrderedByDependencies typedefs = do
    fmap (removeDuplicates . concat) . forM (Map.toList typedefs) $ \(s, t) -> do
        symbols <- getSymbols t
        return (symbols ++ [s])
    where
        getSymbols :: Monad m => Type.Type -> m [Symbol]
        getSymbols typ = case typ of
            Type.Typedef s -> return [s]
            Type.Tuple ts  -> concat <$> mapM getSymbols ts
            Table ts  -> concat <$> mapM getSymbols ts
            Sparse ts  -> concat <$> mapM getSymbols ts
            Type.Array n t -> getSymbols t
            U8  -> return []
            I64 -> return []
            I32 -> return []
            F64 -> return []
            F32 -> return []
            Type.ADT fs -> concat <$> mapM getSymbolsField fs
            Type.Bool -> return []
            _ -> error (show typ)

        getSymbolsField :: Monad m => AdtField -> m [Symbol]
        getSymbolsField field = case field of
            FieldNull -> return []
            FieldType t -> getSymbols t
            FieldCtor ts -> concat <$> mapM getSymbols ts
            _ -> error (show field)
        
        removeDuplicates :: Eq a => [a] -> [a]
        removeDuplicates [] = []
        removeDuplicates (x:xs)
            | elem x xs = x:removeDuplicates (deleteAll x xs)
            | otherwise = x:removeDuplicates xs

        deleteAll :: Eq a => a -> [a] -> [a]
        deleteAll a [] = []
        deleteAll a (x:xs)
            | a == x    = deleteAll a xs
            | otherwise = x : deleteAll a xs


