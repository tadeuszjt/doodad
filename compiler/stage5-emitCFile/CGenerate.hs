{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CGenerate where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map

import Symbol
import ASTResolved hiding (moduleName)
import CBuilder as C hiding (moduleName)
import CAst as C
import Control.Monad.State
import Type
import Error


data GenerateState
    = GenerateState
        { moduleName  :: String
        , structs      :: Map.Map C.Type String
        , supply      :: Map.Map String Int
        , curFnIsRef  :: Bool
        }

initGenerateState moduleName
    = GenerateState
        { moduleName = moduleName
        , structs = Map.empty
        , supply = Map.empty
        , curFnIsRef = False
        }


newtype Generate a = Generate
    { unGenerate :: ReaderT ASTResolved (StateT GenerateState (StateT BuilderState (Except Error))) a }
    deriving (Functor, Applicative, Monad, MonadState GenerateState, MonadReader ASTResolved,
        MonadError Error)

instance MonadBuilder Generate where
    liftBuilderState (StateT s) = Generate $ lift $ lift $ StateT $ pure . runIdentity . s

instance MonadFail Generate where
    fail s = throwError (ErrorStr s)

instance MonadTypeDefs Generate where
    getTypeDefs = typeDefsAll <$> ask



runGenerate :: ASTResolved -> GenerateState -> BuilderState -> Generate a -> Either Error ((a, GenerateState), BuilderState)
runGenerate ast generateState builderState generate =
    runExcept $ runStateT (runStateT (runReaderT (unGenerate generate) ast) generateState) builderState


fresh :: String -> Generate String
fresh suggestion = do
    nm <- Map.lookup suggestion <$> gets supply
    let n = maybe 0 id nm
    modify $ \s -> s { supply = Map.insert suggestion (n + 1) (supply s) }
    return $ suggestion ++ show n


assignRef :: String -> Type.Type -> C.Expression -> Generate C.Expression
assignRef suggestion typ expr = do
    name <- fresh suggestion
    ctyp <- cRefTypeOf typ
    appendElem $ C.Assign ctyp name expr
    return (C.Ident name)


assignVal :: String -> Type.Type -> C.Expression -> Generate C.Expression
assignVal suggestion typ expr = do
    name <- fresh suggestion
    ctyp <- cTypeOf typ
    appendElem $ C.Assign ctyp name expr
    return (C.Ident name)


if_ :: C.Expression -> Generate a -> Generate a
if_ cnd f = do
    id <- appendIf cnd
    withCurID id f


stackRef :: Type.Type -> C.Expression -> Generate C.Expression
stackRef typ cexpr = do
    base <- baseTypeOf typ
    case unfoldType base of
        (Tuple, ts) -> assignRef "ref" typ $ C.Initialiser [C.Address cexpr, C.Int 0, C.Int 1]
        (_, _)      -> case cexpr of
            C.Ident s -> return (C.Address cexpr)
            _         -> assignRef "ref" typ (C.Address cexpr)


deref :: Type.Type -> C.Expression -> Generate C.Expression
deref typ expr = do
    base <- baseTypeOf typ
    case unfoldType base of
        (Tuple, ts) -> do -- {ptr, idx, cap}
            tup <- assignVal "deref" typ $ case ts of
                [] -> C.Initialiser []
                _  -> C.Initialiser [C.Int 0]
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

                appendElem $ C.Set (C.Member tup $ "m" ++ show i) (C.Deref fieldP)

            return tup

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
    Type.Bool      -> return Cbool
    Type.Char      -> return Cchar
    Type.Tuple     -> getTypedef "Tuple" =<< cTypeNoDef (typeof a)

    TypeDef s -> do
        (xs, typ) <- (Map.! s) <$> getTypeDefs
        case xs of
            [] -> return ()
            _  -> error (show xs)
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
            Type.Bool -> return Cbool
            Type.Char -> return Cchar
            Tuple -> return $ Cstruct []

            Apply Type.Slice t -> do
                cType <- cTypeOf t
                return $ Cstruct
                    [ C.Param "ptr" (Cpointer cType)
                    , C.Param "cap" Csize_t
                    , C.Param "start" Csize_t
                    , C.Param "end" Csize_t
                    ]

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


