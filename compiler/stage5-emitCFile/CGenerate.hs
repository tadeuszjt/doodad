{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CGenerate where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map

import Symbol
import ASTResolved hiding (moduleName)
import qualified ASTResolved
import CBuilder as C hiding (moduleName)
import CAst as C
import Control.Monad.State
import Type
import AST as S
import Error

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
        , astResolved :: ASTResolved
        }

initGenerateState ast
    = GenerateState
        { moduleName = ASTResolved.moduleName ast
        , structs = Map.empty
        , supply = Map.empty
        , curFnIsRef = False
        , astResolved = ast
        }


genSymbol :: Symbol -> Generate Symbol
genSymbol symbol@(SymResolved str) = do  
    ast <- gets astResolved
    let modName = (ASTResolved.moduleName ast)
    let im = (Map.lookup symbol $ symSupply ast)
    let n = maybe 0 (id) im

    modify $ \s -> s { astResolved = ast { symSupply = Map.insert symbol (n + 1) (symSupply ast) } }
    return $ SymResolved ([modName] ++ str ++ [show n])


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


fresh :: String -> Generate String
fresh suggestion = do
    nm <- Map.lookup suggestion <$> gets supply
    let n = maybe 0 id nm
    modify $ \s -> s { supply = Map.insert suggestion (n + 1) (supply s) }
    return $ suggestion ++ show n


assign :: String -> Value -> Generate Value
assign suggestion val = do
    case unfoldType (typeof val) of
        (Type.Slice, _) -> fail "cannot assign slice"
        _ -> return ()

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


makeRef :: Value -> Generate C.Expression
makeRef val = do
    base <- baseTypeOf val
    case val of
        Value _ _ -> case unfoldType base of
            (Tuple, ts) -> fmap refExpr $ assign "ref" $ Ref (typeof val) $ C.Initialiser
                [ C.Address (valExpr val)  -- ptr
                , C.Int 0                  -- idx
                , C.Int 1                  -- cap
                ]

            (_, _) -> fmap refExpr $ assign "ref" $ Ref (typeof val) $ C.Address (valExpr val)

        Ref _ _ -> return (refExpr val)


deref :: Type.Type -> C.Expression -> Generate C.Expression
deref typ expr = do
    base <- baseTypeOf typ
    case unfoldType base of
        (Tuple, ts) -> do -- {ptr, idx, cap}
            tup <- assign "deref" $ Value typ $ C.Initialiser [C.Int 0]
            cts <- mapM cTypeOf ts
            ct <- cTypeOf base
            forM_ (zip ts [0..]) $ \(t, i) -> do
                
                let off = C.Offsetof ct ("m" ++ show i)
                let idx = C.Member expr "idx"
                let cap = C.Member expr "cap"
                let ptr = C.Cast (Cpointer Cvoid) (C.Member expr "ptr")

                let size = C.SizeofType (cts !! i)
                let offset = C.Infix C.Plus (C.Infix C.Times cap off) (C.Infix C.Times idx size)
                let fieldP = C.Cast (Cpointer $ cts !! i) (C.Infix C.Plus ptr offset)

                appendElem $ C.Set (C.Member (valExpr tup) $ "m" ++ show i) (C.Deref fieldP)

            return (valExpr tup)

        _ -> return (C.Deref expr)
    

cRefTypeOf :: Typeof a => a -> Generate C.Type
cRefTypeOf a = do
    base <- baseTypeOf a -- use base makes conversions simpler
    case unfoldType base of
        (x, []) | isSimple x -> Cpointer <$> cTypeOf base
        (Table, _)           -> Cpointer <$> cTypeOf base
        (Sum, _)             -> Cpointer <$> cTypeOf base
        (Type.Array, _)      -> Cpointer <$> cTypeOf base

        (Tuple, ts) -> do
            pt <- Cpointer <$> cTypeOf base
            cst <- return $ Cstruct [C.Param "ptr" pt, C.Param "idx" Csize_t, C.Param "cap" Csize_t]
            getTypedef "WideRef" cst

        x -> fail $ "cannot create cRefType of: " ++  (show x)


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
                return $ Cstruct [C.Param "ptr" (Cpointer cType), C.Param "len" Csize_t, C.Param "cap" Csize_t]

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


