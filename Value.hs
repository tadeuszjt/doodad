{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Value where

import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Short      as BSS
import           Data.Maybe
import           Data.List                  hiding (and, or)
import           Data.Word
import qualified Data.Map                   as Map
import           Prelude                    hiding (EQ, and, or)

import           LLVM.Context
import           LLVM.AST                   hiding (Type, function, Module)
import qualified LLVM.AST                   as LL (Type) 
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Type              hiding (Type, void, double)
import           LLVM.Internal.Type
import           LLVM.Internal.EncodeAST
import           LLVM.Internal.Coding           hiding (alloca)
import           Foreign.Ptr
import qualified LLVM.Internal.FFI.DataLayout   as FFI
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified AST                        as S
import           Type
import           CmpFuncs
import           CmpMonad

mkBSS = BSS.toShort . BS.pack


type Compile    = StateT CompileState IO
type MyCmpState = CmpState SymKey SymObj
type Instr      = InstrCmpT SymKey SymObj Compile
type Module     = ModuleCmpT SymKey SymObj Compile


data CompileState
    = CompileState
        { context     :: Context
        , dataLayout  :: Ptr FFI.DataLayout
        , curRetTyp   :: Type
        , expressions :: Map.Map String S.Expr
        }
initCompileState ctx dl exprs = CompileState ctx dl Void exprs


setCurRetTyp :: Type -> Instr ()
setCurRetTyp typ =
    lift $ modify $ \s -> s { curRetTyp = typ }


getCurRetTyp :: Instr Type
getCurRetTyp =
    lift (gets curRetTyp)


data SymKey
    = KeyVal
    | KeyFunc [Type]
    | KeyType
    | KeyDataCons
    deriving (Show, Eq, Ord)


instance Show ([Value] -> Instr Value) where show _ = "Inline"
instance Show (Value -> Instr ()) where show _ = "Inline"
data SymObj
    = ObjVal Value
    | ObjFunc Type Operand
    | ObjInline ([Value] -> Instr Value)
    | ObjType Type
    | ObjDataCons { dataTyp :: Type, dataConsTyp :: Type, dataEnum :: Word }
    | ObjData     { dataConcTyp :: Type, dataPrintFn :: (Value -> Instr ()) }
    deriving (Show)


data Value
    = Val { valType :: Type, valOp :: Operand }
    | Ptr { valType :: Type, valOp :: Operand }
    deriving (Show, Eq)


valInt :: Type -> Integer -> Value
valInt I8 n  = Val I8 (int8 n)
valInt I32 n = Val I32 (int32 n)
valInt I64 n = Val I64 (int64 n)


valBool :: Bool -> Value
valBool b = Val Bool (if b then bit 1 else bit 0)


opTypeOf :: Type -> Instr LL.Type
opTypeOf (Tuple nm ts) =
    if isNothing nm
    then fmap (StructureType False) (mapM opTypeOf ts)
    else do
        let name = fromJust nm
        ensureDef name
        return (NamedTypeReference name)
opTypeOf (Table nm ts) =
    if isNothing nm
    then do
        opTyps <- mapM (opTypeOf) ts
        return $ StructureType False $ i64:i64:(map ptr opTyps)
    else do
        let name = fromJust nm
        ensureDef name
        return (NamedTypeReference name)
opTypeOf (Typedef sym) = do
    res <- look sym KeyType
    case res of
        ObjType t   -> opTypeOf t
        ObjData t _ -> opTypeOf t
opTypeOf typ = case typ of
    Void        -> return VoidType
    I8          -> return i8
    I32         -> return i32
    I64         -> return i64
    F32         -> return (FloatingPointType HalfFP)
    F64         -> return (FloatingPointType DoubleFP)
    Bool        -> return i1
    Char        -> return i32
    String      -> return (ptr i8)
    Array n t   -> fmap (ArrayType $ fromIntegral n) (opTypeOf t)
    Annotated _ t -> opTypeOf t


-- sizeOf cannot know about typedefs!
sizeOf :: Type -> Instr Word64
sizeOf typ = size =<< opTypeOf =<< concreteTypeOf typ
    where
        size :: LL.Type -> Instr Word64
        size t = lift $ do
            dl <- gets dataLayout
            ctx <- gets context
            ptrTyp <- liftIO $ runEncodeAST ctx (encodeM t)
            liftIO (FFI.getTypeAllocSize dl ptrTyp)


zeroOf :: Type -> Instr C.Constant
zeroOf typ = case typ of
    I8            -> return $ toCons (int8 0)
    I32           -> return $ toCons (int32 0)
    I64           -> return $ toCons (int64 0)
    F32           -> return $ toCons (single 0)
    F64           -> return $ toCons (double 0)
    Char          -> return $ toCons (int32 0)
    Bool          -> return $ toCons (bit 0)
    String        -> return $ C.IntToPtr (toCons $ int64 0) (ptr i8)
    Array n t     -> fmap (toCons . array . replicate (fromIntegral n)) (zeroOf t)
    Table nm ts   -> do
        let zi64 = toCons (int64 0)
        ptrs <- fmap (map ptr) (mapM opTypeOf ts)
        let zptrs = map (C.IntToPtr zi64) ptrs
        return . toCons $ struct nm False (zi64:zi64:zptrs)
    Tuple nm ts   -> fmap (toCons . (struct Nothing False)) (mapM zeroOf ts)
    Typedef _     -> zeroOf =<< realTypeOf typ
    Annotated _ t -> zeroOf t


realTypeOf :: Type -> Instr Type
realTypeOf typ = case typ of
    Annotated _ t -> realTypeOf t
    Typedef sym   -> do
        res <- look sym KeyType
        case res of
            ObjType t   -> realTypeOf t
            ObjData t _ -> realTypeOf t
    t             -> return t


concreteTypeOf :: Type -> Instr Type
concreteTypeOf (Typedef sym) = do
    res <- look sym KeyType
    case res of
        ObjType t   -> concreteTypeOf t
        ObjData _ _ -> return (dataConcTyp res)
concreteTypeOf typ = case typ of
    Table nm ts   -> fmap (Table Nothing) (mapM concreteTypeOf ts)
    Tuple nm ts   -> fmap (Tuple Nothing) (mapM concreteTypeOf ts)
    Array n t     -> fmap (Array n) (concreteTypeOf t)
    Annotated _ t -> concreteTypeOf t
    t             -> return t


-- Same type
-- Ignore annotations
-- Recursively check aggregates
checkTypesMatch :: Type -> Type -> Instr ()
checkTypesMatch a b = case (a, b) of
    (Annotated _ a, b)       -> checkTypesMatch a b
    (a, Annotated _ b)       -> checkTypesMatch a b
    (Tuple _ as, Tuple _ bs) -> do
        assert (length as == length bs) "tuple length mismatch"
        mapM_ (\(x, y) -> checkTypesMatch x y) (zip as bs)
    (Array an at, Array bn bt) -> do
        assert (an == bn) "array length mismatch"
        checkTypesMatch at bt
    (Table _ as, Table _ bs)   -> do
        assert (length as == length bs) "table type mismatch"
        mapM_ (\(x, y) -> checkTypesMatch x y) (zip as bs)
    (a, b) | isTypedef a || isSimple a -> return ()
    (a, b) -> assert (a == b) ("type mismatch between " ++ show a ++ " and " ++ show b)


checkConcTypesMatch :: Type -> Type -> Instr ()
checkConcTypesMatch a b = do
    a' <- concreteTypeOf a
    b' <- concreteTypeOf b
    assert (a' == b') ("underlying type mismatch between " ++ show a' ++ " and " ++ show b')


ensureTypeDeps :: Type -> Instr ()
ensureTypeDeps typ = case typ of
    Array _ t -> ensureTypeDeps t
    Tuple nm ts -> do
        maybe (return ()) ensureDef nm
        mapM_ ensureTypeDeps ts
    Table nm ts -> do
        maybe (return ()) ensureDef nm
        mapM_ ensureTypeDeps ts
    Typedef sym -> do
        res <- look sym KeyType
        case res of
            ObjType t   -> ensureTypeDeps t
            ObjData t _ -> ensureTypeDeps t
    _ -> return ()


valGlobal :: Name -> Type -> Instr (Value, Definition)
valGlobal name typ = do
    opTyp <- opTypeOf typ
    zero <- zeroOf typ
    let (def, loc) = globalDef name opTyp (Just zero)
    let (ext, _) = globalDef name opTyp Nothing
    emitDefn def
    return (Ptr typ loc, ext)


valLocal :: Type -> Instr Value
valLocal typ = do
    opTyp <- opTypeOf typ
    loc <- alloca opTyp Nothing 0
    size <- sizeOf typ
    memset loc (int64 0) $ int64 (fromIntegral size)
    return (Ptr typ loc)


valStore :: Value -> Value -> Instr ()
valStore (Ptr typ loc) val = do
    checkTypesMatch typ (valType val)
    case val of
        Ptr t l -> store loc 0 =<< load l 0
        Val t o -> store loc 0 o


valLoad :: Value -> Instr Value
valLoad (Val typ op)  = return (Val typ op)
valLoad (Ptr typ loc) = fmap (Val typ) (load loc 0)


valCast :: Type -> Value -> Instr Value
valCast typ' (Ptr typ loc) = do
    opTyp' <- opTypeOf typ'
    loc' <- bitcast loc (ptr opTyp')
    return (Ptr typ' loc')
valCast typ' (Val typ op) = do
    opTyp' <- opTypeOf typ'
    op' <- bitcast op opTyp'
    return (Val typ' op')


valPtrIdx :: Value -> Value -> Instr Value
valPtrIdx (Ptr typ loc) (Val I64 i) = fmap (Ptr typ) (gep loc [i])
valPtrIdx (Ptr typ loc) (Ptr I64 i) = valPtrIdx (Ptr typ loc) =<< valLoad (Ptr I64 i)


valPtrMemCpy :: Value -> Value -> Value -> Instr ()
valPtrMemCpy (Ptr ta pa) (Ptr tb pb) (Val I64 n) = do
    checkTypesMatch ta tb
    size <- sizeOf ta
    nbytes <- mul n $ int64 (fromIntegral size)
    void (memcpy pa pb nbytes)


valArrayIdx :: Value -> Value -> Instr Value
valArrayIdx (Ptr (Array n t) loc) idx = do
    assert (isInt $ valType idx) "array index isn't int"
    Val _ i <- valLoad idx
    ptr <- gep loc [int64 0, i]
    return (Ptr t ptr)


valArrayConstIdx :: Value -> Word -> Instr Value
valArrayConstIdx val i = do
    Array n t <- realTypeOf (valType val)
    case val of
        Ptr typ loc -> fmap (Ptr t) $ gep loc [int64 0, int64 (fromIntegral i)]
        Val typ op  -> fmap (Val t) $ extractValue op [fromIntegral i]


valArraySet :: Value -> Value -> Value -> Instr ()
valArraySet (Ptr typ loc) idx val = do
    Array n t <- realTypeOf typ
    assert (isInt $ valType idx) "index isn't int"
    assert (valType val == t) "incorrect element type"
    i <- valLoad idx
    ptr <- gep loc [int32 0, valOp i]
    valStore (Ptr t ptr) val


valTableLen :: Value -> Instr Value
valTableLen (Ptr _ loc) = fmap (Ptr I64) $ gep loc [int32 0, int32 0]
valTableLen (Val _ op)  = fmap (Val I64) $ extractValue op [0]


valLen :: Value -> Instr Word
valLen val = do
    typ <- concreteTypeOf (valType val)
    case typ of
        Array n t   -> return n
        Tuple nm ts -> return $ fromIntegral (length ts)


valTupleIdx :: Word32 -> Value -> Instr Value
valTupleIdx i tup = do
    Tuple nm ts <- realTypeOf (valType tup)
    assert (i >= 0 && fromIntegral i < length ts) "tuple index out of range"
    let t = ts !! fromIntegral i
    case tup of
        Ptr _ loc -> fmap (Ptr t) (gep loc [int32 0, int32 (fromIntegral i)])
        Val _ op  -> fmap (Val t) (extractValue op [i])
    

valTupleSet :: Value -> Word32 -> Value -> Instr ()
valTupleSet (Ptr typ loc) i val = do
    Tuple nm ts <- realTypeOf typ
    p <- gep loc [int32 0, int32 (fromIntegral i)]
    valStore (Ptr (ts !! fromIntegral i) p) val


valsEqual :: Value -> Value -> Instr Value
valsEqual a b = do
    checkTypesMatch (valType a) (valType b)
    typ <- concreteTypeOf (valType a)
    fmap (Val Bool) $ valsEqual' typ a b
    where
        valsEqual' :: Type -> Value -> Value -> Instr Operand
        valsEqual' typ a b
            | isIntegral typ || typ == Bool = do
                Val _ opA <- valLoad a
                Val _ opB <- valLoad b
                icmp EQ opA opB

        valsEqual' typ a b = error $ show (typ, a, b)


valAnd :: [Value] -> Instr Value
valAnd [val] = do
    assert (valType val == Bool) "non-bool in and expression"
    valLoad val
valAnd (v:vs) = do
    Val Bool op  <- valAnd [v]
    Val Bool ops <- valAnd vs
    fmap (Val Bool) (and ops op)


