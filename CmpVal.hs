{-# LANGUAGE TupleSections #-}

module CmpVal where

import           Control.Monad
import           Control.Monad.Except       hiding (void)
import           Control.Monad.State
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Short      as BSS
import           Data.Maybe
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.List                  hiding (and, or)
import           Data.Word
import           Prelude                    hiding (EQ, and, or)

import           LLVM.AST                   hiding (function, Module)
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.FloatingPointPredicate (FloatingPointPredicate(OEQ))
import           LLVM.AST.Type              hiding (void, double)
import           LLVM.AST.Typed
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified Lexer                      as L
import qualified AST                        as S
import           CmpBuilder
import           Cmp

mkBSS = BSS.toShort . BS.pack


type Compile    = State CompileState
type MyCmpState = CmpState SymKey SymObj
type Instr      = InstrCmpT SymKey SymObj Compile
type Module     = ModuleCmpT SymKey SymObj Compile


type Value = (ValType, Operand)
type Func  = ([ValType], ValType, Operand)


data CompileState
    = CompileState
        { curRetType :: ValType
        }
    deriving (Show, Eq)


initCompileState
    = CompileState
        { curRetType = Void
        }


data SymKey
    = KeyVal
    | KeyFunc [ValType]
    | KeyType
    deriving (Show, Eq, Ord)


data SymObj
    = ObjVal Value
    | ObjFunc ValType Operand
    | ObjType ValType
    deriving (Show, Eq)


data ValType
    = Void
    | I32
    | I64
	| F32
	| F64
    | Bool
    | Char
    | String
    | Tuple [ValType]
    | ArrayPtr Int ValType
    | ArrayVal Int ValType
    | Typedef String
    deriving (Show, Eq, Ord)


isInt x                             = x `elem` [I32, I64]
isFloat x                           = x `elem` [F32, F64]
isArray (ArrayVal _ _)              = True
isArray (ArrayPtr _ _)              = True
isArray _                           = False
isTuple (Tuple _)                   = True
isTuple _                           = False
isExpr (Typedef _)                  = True
isExpr x
    | isInt x || isFloat x          = True
    | isArray x || isTuple x        = True
    | x `elem` [Bool, Char, String] = True
isExpr _                            = False


fromASTType :: TextPos -> S.Type -> Instr ValType
fromASTType pos typ = case typ of
    S.TBool      -> return Bool
    S.TI32       -> return I32
    S.TI64       -> return I64
    S.TF32       -> return F32
    S.TF64       -> return F64
    S.TChar      -> return Char
    S.TString    -> return String
    S.TArray n t -> fmap (ArrayVal n) (fromASTType pos t)
    S.TTuple ts  -> fmap Tuple $ mapM (fromASTType pos) ts
    S.TIdent sym -> return (Typedef sym)


opTypeOf :: TextPos -> ValType -> Instr Type
opTypeOf pos typ = case typ of
        Void         -> error "opTypeOf void"
        Bool         -> return i1
        Char         -> return i32
        I32          -> return i32
        I64          -> return i64
        F32          -> return (FloatingPointType HalfFP)
        F64          -> return (FloatingPointType DoubleFP)
        Tuple typs   -> fmap (StructureType False) $ mapM (opTypeOf pos) typs
        ArrayPtr n t -> fmap (ptr . (ArrayType $ fromIntegral n)) (opTypeOf pos t)
        ArrayVal n t -> fmap (ArrayType $ fromIntegral n) (opTypeOf pos t)
        String       -> return (ptr i8)
        Typedef sym  -> opTypeOf pos =<< getConcreteType pos typ


zeroOf :: ValType -> C.Constant
zeroOf typ = case typ of
    I32          -> toCons (int32 0)
    I64          -> toCons (int64 0)
    F32          -> toCons (single 0)
    F64          -> toCons (double 0)
    Bool         -> toCons (bit 0)
    Char         -> toCons (int32 0)
    String       -> C.IntToPtr (toCons $ int64 0) (ptr i8)
    ArrayVal n t -> toCons $ array $ replicate n (zeroOf t)
    Tuple typs   -> toCons $ struct Nothing False (map zeroOf typs)


valGlobalClone :: TextPos -> Name -> Value -> Instr Operand
valGlobalClone pos name val@(typ@(ArrayPtr n t), op) = do
    (t, op) <- valFlatten pos val
    opTyp <- opTypeOf pos typ
    loc <- global name opTyp (zeroOf typ)
    store loc 0 op
    return loc
valGlobalClone pos name (typ, op) = do
    opTyp <- opTypeOf pos typ
    loc <- global name opTyp (zeroOf typ)
    store loc 0 op
    return loc
    

valFlatten :: TextPos -> Value -> Instr Value
valFlatten pos val@(ArrayPtr n t, loc)   = fmap (ArrayVal n t,) (load loc 0)
valFlatten pos val@(typ@(Typedef _), op) = getConcreteType pos typ >>= \t -> valFlatten pos (t, op)
valFlatten pos val                       = return val


getConcreteType :: TextPos -> ValType -> Instr ValType
getConcreteType pos (Typedef symbol) = do
    ObjType typ <- look pos symbol KeyType
    getConcreteType pos typ
getConcreteType pos typ = return typ


typesMatch :: TextPos -> ValType -> ValType -> Instr Bool
typesMatch pos a b = do
    ca <- getConcreteType pos a
    cb <- getConcreteType pos b
    return (ca == cb)


valPrint :: Value -> Instr ()
valPrint (Bool, op) = do
    str <- globalStringPtr "true\0false" =<< fresh
    idx <- select op (int64 0) (int64 5)
    ptr <- gep (cons str) [idx]
    void (printf "%s" [ptr])
valPrint val@(typ, op)
    | isArray typ = do
        let len = valArrayLen val
        putchar '['
        forM_ [0..len-1] $ \i -> do
            valPrint =<< valArrayConstIdx val i
            when (i < len-1) $ void (printf ", " [])
        void (putchar ']')
valPrint val@(Tuple ts, op) = do
    let len = length ts
    putchar '('
    forM_ [0..len-1] $ \i -> do
        valPrint =<< valTupleIdx (TextPos 0 0 0) val i
        when (i < len-1) $ void (printf ", " [])
    void (putchar ')')
valPrint val@(typ, op) = case typ of
    I32    -> void (printf "%d" [op])
    I64    -> void (printf "%ld" [op])
    F32    -> void (printf "f" [op])
    F64    -> void (printf "%f" [op])
    Char   -> void (putchar' op)
    String -> void (printf "%s" [op])




valArrayToPtr :: Value -> Instr Value
valArrayToPtr (ArrayVal n t, arr) = do
    loc <- alloca (typeOf arr) Nothing 0
    store loc 0 arr
    return (ArrayPtr n t, loc)


valArrayIdx :: Value -> Value -> Instr Value
valArrayIdx val@(ArrayVal n t, _) idx = do
    arr <- valArrayToPtr val
    valArrayIdx arr idx
valArrayIdx (ArrayPtr n t, loc) (idxTyp, idx) = do
    unless (isInt idxTyp) (error "wasn't int")
    ptr <- gep loc [int64 0, idx]
    op <- load ptr 0
    return (t, op)


valArrayConstIdx :: Value -> Int -> Instr Value
valArrayConstIdx (ArrayVal n t, arr) i = do
    op <- extractValue arr [fromIntegral i]
    return (t, op)
valArrayConstIdx (ArrayPtr n t, loc) i = do
    ptr <- gep loc [int64 0, (int64 $ fromIntegral i)]
    op <- load ptr 0
    return (t, op)


valArrayLen :: Value -> Int
valArrayLen (ArrayPtr n _, _) = n
valArrayLen (ArrayVal n _, _) = n


valTupleIdx :: TextPos -> Value -> Int -> Instr Value
valTupleIdx pos (Tuple typs, op) i = do
    unless (i >= 0 && i < length typs) (cmpErr pos "tuple index out of range")
    o <- extractValue op [fromIntegral i]
    return (typs !! i, o)
    

valsEqual :: TextPos -> Value -> Value -> Instr Value
valsEqual pos (aTyp_, aOp) (bTyp_, bOp) = do
    aTyp <- getConcreteType pos aTyp_
    bTyp <- getConcreteType pos bTyp_
    unless (aTyp == bTyp) (cmpErr pos "types don't match")
    op <- opEquality aTyp aOp bOp
    return (Bool, op)
    where
        opEquality :: ValType -> Operand -> Operand -> Instr Operand
        opEquality typ aOp bOp = case typ of
            I32      -> icmp EQ aOp bOp
            I64      -> icmp EQ aOp bOp
            F32      -> fcmp OEQ aOp bOp
            F64      -> fcmp OEQ aOp bOp
            Bool     -> icmp EQ aOp bOp
            Char     -> icmp EQ aOp bOp
            String   -> icmp EQ (int32 0) =<< strcmp aOp bOp
            Tuple ts -> do
                cnds <- forM (zip ts [0..]) $ \(t, i) -> do
                    a <- extractValue aOp [i]
                    b <- extractValue bOp [i]
                    opEquality t a b
                cnd <- alloca i1 Nothing 0
                forM_ cnds (\c -> store cnd 0 =<< and c =<< load cnd 0)
                load cnd 0
            _         -> cmpErr pos "invalid comparison"
        

