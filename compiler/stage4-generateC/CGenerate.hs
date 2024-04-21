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
        { moduleName :: String
        , tuples     :: Map.Map C.Type String
        , supply     :: Map.Map String Int
        , typefuncs  :: Map.Map Symbol ([Symbol], Type.Type)
        , refFuncs   :: Map.Map Symbol Bool
        , symTab     :: SymTab.SymTab String () Value
        }

initGenerateState modName
    = GenerateState
        { moduleName = modName
        , tuples = Map.empty
        , supply = Map.empty
        , typefuncs = Map.empty
        , symTab = SymTab.initSymTab
        , refFuncs = Map.empty
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
    return $ refFuncs Map.! symbol


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

        Type.TypeApply (Sym "Sum") ts -> do
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
        Type.TypeApply (Sym "Sum") _ -> return $ Value typ (C.Deref expr)
        Type.TypeApply (Sym "Tuple") ts  -> do
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

        (Ref _ a, Ref _ b) -> case base of
            Type.TypeApply (Sym "Tuple") ts -> do
                -- TODO implement shear
                let tupA = C.Deref (C.Member a "ptr")
                let tupB = C.Deref (C.Member b "ptr")

                forM_ (zip ts [0..]) $ \(t, i) -> do
                    let va = Value t $ C.Member tupA ("m" ++ show i)
                    let vb = Value t $ C.Member tupB ("m" ++ show i)
                    set va vb
            Type.TypeApply (Sym "Sum") ts | copyable -> do
                void $ appendElem $ C.Set (C.Deref a) (C.Deref b)

            x -> error (show x)

        (Ref _ a, Value _ b) -> case base of
            x | isSimple x -> set (Value typ $ C.Deref a) (Value typ b)

            Type.TypeApply (Sym "Tuple") ts -> do
                -- TODO implement shear
                let tupA = C.Deref (C.Member a "ptr")
                let tupB = b

                forM_ (zip ts [0..]) $ \(t, i) -> do
                    let va = Value typ $ C.Member tupA ("m" ++ show i)
                    let vb = Value typ $ C.Member tupB ("m" ++ show i)
                    set va vb

            Type.TypeApply (Sym "Sum") ts | copyable ->
                void $ appendElem $ C.Set (C.Deref a) b

            x -> error (show x)

        (Value _ a, Value _ b) -> case base of
            Type.TypeApply (Sym "Tuple") ts -> do
                forM_ (zip ts [0..]) $ \(t, i) -> do
                    let va = Value t $ C.Member a ("m" ++ show i)
                    let vb = Value t $ C.Member b ("m" ++ show i)
                    set va vb

            Type.TypeApply (Sym "Table") t -> do
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
            Type.String               -> Value I64 (C.Call "strlen" [expr])
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
        Type.TypeApply (Sym "Tuple") ts -> do
            unless (idx >= 0 && idx < length ts) (error "invalid member index")
            -- TODO implement shear
            return $ Value (ts !! idx) $ C.PMember (C.Member expr "ptr") ("m" ++ show idx)
        Type.TypeApply (Sym "Sum") ts   -> do
            unless (idx >= 0 && idx < length ts) (error "invalid Sum field index")
            return $ Value (ts !! idx) $ C.PMember expr ("u" ++ show idx)
        x -> error (show x)
member idx val = do
    base <- baseTypeOf val
    case base of
        Type.TypeApply (Sym "Tuple") ts -> do
            unless (idx >= 0 && idx < length ts) (error "invalid member index")
            return $ Value (ts !! idx) $ C.Member (valExpr val) ("m" ++ show idx)

        Type.TypeApply (Sym "Table") t -> do
            error ""
--            Type.Record ts <- baseTypeOf t
--            Type.RecordTree ns <- getRecordTree t
--            leaves <- getRecordLeaves (ns !! idx)
--            let elems  = map (\(t, i) -> C.Member (valExpr val) ("r" ++ show i)) leaves
--            assign "member" $ Value (TypeApply (Sym "Table") $ ts !! idx) $ C.Initialiser $
--                [ C.Member (valExpr val) "len"
--                , C.Member (valExpr val) "cap"
--                ] ++ elems

        Type.TypeApply (Sym "Sum") ts   -> do
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
        Type.TypeApply (Sym "Tuple") ts -> do
            unless (length vals == length ts) (fail "invalid tuple initialiser")
            assign "zero" $ Value typ $ C.Initialiser (map valExpr vals)

        _ -> error (show base)


builtinTableGet :: Value -> Value -> Generate Value
builtinTableGet val idx = do
    TypeApply (Sym "Table") [t] <- baseTypeOf val
    baseT <- baseTypeOf t
    case baseT of
        _ | isSimple baseT -> return $ Value t $ C.Subscript
            (C.Member (valExpr val) "r0")
            (valExpr idx)
        Type.TypeApply (Sym "Tuple") _ -> error ""

            
        TypeApply (Sym "Sum") ts -> error ""
        Type.TypeApply (Sym "Table") _ -> error ""
        x -> error (show x)



builtinTableAt :: Value -> Value -> Generate Value
builtinTableAt (Ref tabType expr) idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    TypeApply (Sym "Table") [t] <- baseTypeOf tabType
    baseT <- baseTypeOf t
    case baseT of
        x | isSimple x -> return $ Ref t $ C.Address $ C.Subscript
            (C.PMember expr "r0")
            (valExpr idx)

        Type.TypeApply (Sym "Table") [_] -> return $ Ref t $ C.Address $ C.Subscript
            (C.PMember expr "r0")
            (valExpr idx)
            

        Type.TypeApply (Sym "Tuple") ts -> do
            -- TODO implement shear
            let ptr = C.Address $ C.Subscript (C.PMember expr "r0") (valExpr idx)
            assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

        x -> error (show x)


isCopyable :: Type.Type -> Generate Bool
isCopyable typ = do
    base <- baseTypeOf typ
    case base of
        x | isSimple x                  -> return True
        Type.TypeApply (Sym "Tuple") ts -> all id <$> mapM isCopyable ts
        Type.TypeApply (Sym "Sum")   ts -> all id <$> mapM isCopyable ts
        x -> error (show x)


cParamOf :: S.Param -> Generate C.Param
cParamOf param = do
    ctype <- case param of
        S.Param _ _ _ -> cTypeOf param
        S.RefParam _ _ _ -> cRefTypeOf param
    return $ C.Param { C.cName = show (paramName param), C.cType = ctype }


cRefTypeOf :: Typeof a => a -> Generate C.Type
cRefTypeOf a = do
    base <- baseTypeOf a
    case base of
        x | isSimple x -> Cpointer <$> cTypeOf a 
        Type.TypeApply (Sym "Table") _  -> Cpointer <$> cTypeOf a
        Type.TypeApply (Sym "Sum") ts   -> Cpointer <$> cTypeOf a
        Type.TypeApply (Sym "Tuple") ts -> do
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
    Type.String    -> return (Cpointer Cchar)

    Type.Slice t                     -> getTypedef "Slice"  =<< cTypeNoDef (Type.Slice t)
    Type.TypeApply (Sym "Tuple") t   -> getTypedef "Tuple"  =<< cTypeNoDef (Type.TypeApply (Sym "Tuple") t)
    Type.TypeApply (Sym "Table") t   -> getTypedef "Table"  =<< cTypeNoDef (Type.TypeApply (Sym "Table") t)
    Type.TypeApply (Sym "Sum") ts    -> getTypedef "Adt"    =<< cTypeNoDef (Type.TypeApply (Sym "Sum") ts)

    Type.TypeApply symbol args -> do
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
            Type.String -> return (Cpointer Cchar)
            Type.TypeApply (Sym "Tuple") ts -> do
                cts <- mapM cTypeOf ts
                return $ Cstruct $ map (\(ct, i) -> C.Param ("m" ++ show i) ct) (zip cts [0..])

            Type.TypeApply (Sym "Sum") ts -> do
                cts <- mapM cTypeOf ts
                return $ Cstruct [C.Param "en" Cint64_t, C.Param "" $
                    Cunion $ map (\(ct, i) -> C.Param ("u" ++ show i) ct) (zip cts [0..])]

            Type.TypeApply (Sym "Table") [t] -> do
                baseT <- baseTypeOf t
                cts <- mapM cTypeOf =<< case baseT of
                    t              -> return [t]
                let pts = zipWith (\ct i -> C.Param ("r" ++ show i) (Cpointer ct)) cts [0..]
                return $ Cstruct (C.Param "len" Cint64_t:C.Param "cap" Cint64_t:pts)

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
