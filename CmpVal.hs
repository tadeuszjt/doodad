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


fromASTType :: S.Type -> Instr ValType
fromASTType typ = case typ of
    S.TBool      -> return Bool
    S.TI32       -> return I32
    S.TI64       -> return I64
    S.TF32       -> return F32
    S.TF64       -> return F64
    S.TChar      -> return Char
    S.TString    -> return String
    S.TArray n t -> fmap (ArrayVal n) (fromASTType t)
    S.TTuple ts  -> fmap Tuple (mapM fromASTType ts)
    S.TIdent sym -> return (Typedef sym)


opTypeOf :: ValType -> Instr Type
opTypeOf typ = case typ of
        Void         -> error "opTypeOf void"
        Bool         -> return i1
        Char         -> return i32
        I32          -> return i32
        I64          -> return i64
        F32          -> return (FloatingPointType HalfFP)
        F64          -> return (FloatingPointType DoubleFP)
        Tuple typs   -> fmap (StructureType False) (mapM opTypeOf typs)
        ArrayPtr n t -> fmap (ptr . (ArrayType $ fromIntegral n)) (opTypeOf t)
        ArrayVal n t -> fmap (ArrayType $ fromIntegral n) (opTypeOf t)
        String       -> return (ptr i8)
        Typedef sym  -> opTypeOf =<< getConcreteType typ


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


valGlobalClone :: Name -> Value -> Instr (ValType, Operand, Definition)
valGlobalClone name val@(typ@(ArrayPtr n t), _) = do
    (flatTyp@(ArrayVal _ _), op) <- valFlatten val
    opTyp <- opTypeOf flatTyp
    loc <- global name opTyp (zeroOf flatTyp)
    store loc 0 op
    return (ArrayPtr n t, loc, globalDef name opTyp Nothing)

valGlobalClone name (typ, op) = do
    opTyp <- opTypeOf typ
    loc <- global name opTyp (zeroOf typ)
    store loc 0 op
    return (typ, loc, globalDef name opTyp Nothing)


valLoad :: Value -> Instr Value
valLoad val@(ArrayPtr _ _, _) = return val
valLoad (typ, loc) = do
    op <- load loc 0
    return (typ, op)
    

valFlatten :: Value -> Instr Value
valFlatten val@(ArrayPtr n t, loc) = do
    arrValOpTyp <- opTypeOf (ArrayVal n t)
    newLoc <- alloca arrValOpTyp Nothing 0
    for (int64 $ fromIntegral n) $ \i -> do
        ptr <- gep loc [int64 0, i]
        elm <- valFlatten =<< valLoad (t, ptr)
        valArraySet (ArrayPtr n t, newLoc) (I64, i) elm
    fmap (ArrayVal n t,) (load newLoc 0)
valFlatten val@(typ@(Typedef _), op) = getConcreteType typ >>= \t -> valFlatten (t, op)
valFlatten val                       = return val


getConcreteType :: ValType -> Instr ValType
getConcreteType (Typedef symbol) = do
    ObjType typ <- look symbol KeyType
    getConcreteType typ
getConcreteType typ = return typ


typesMatch :: ValType -> ValType -> Instr Bool
typesMatch a b = do
    ca <- getConcreteType a
    cb <- getConcreteType b
    return (ca == cb)


valPrint :: String -> Value -> Instr ()
valPrint append (Bool, op) = do
    str <- globalStringPtr "true\0false" =<< fresh
    idx <- select op (int64 0) (int64 5)
    ptr <- gep (cons str) [idx]
    void $ printf ("%s" ++ append) [ptr]
valPrint append val@(typ, op)
    | isArray typ = do
        let len = valArrayLen val
        putchar '['
        forM_ [0..len-1] $ \i -> do
            let app = if i < len-1 then ", " else "]"++append
            valPrint app =<< valArrayConstIdx val i
valPrint append val@(Tuple ts, op) = do
    let len = length ts
    putchar '('
    forM_ [0..len-1] $ \i -> do
        let app = if i < len-1 then ", " else ")"++append
        valPrint app =<< valTupleIdx val i
valPrint append val@(typ, op) =
    case typ of
        I32    -> void $ printf ("%d"++append) [op]
        I64    -> void $ printf ("%ld"++append) [op]
        F32    -> void $ printf ("f"++append) [op]
        F64    -> void $ printf ("%f"++append) [op]
        Char   -> void $ printf ("%c"++append) [op]
        String -> void $ printf ("%s"++append) [op]




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


valArraySet :: Value -> Value -> Value -> Instr ()
valArraySet (ArrayPtr n t, loc) (idxTyp, idxOp) val = do
    assert (isInt idxTyp) "index isn't int"
    (typ, op) <- valFlatten val
    assert (typ == t) "incorrect element type"
    ptr <- gep loc [int64 0, idxOp]
    store ptr 0 op
    

valArrayLen :: Value -> Int
valArrayLen (ArrayPtr n _, _) = n
valArrayLen (ArrayVal n _, _) = n


valTupleIdx :: Value -> Int -> Instr Value
valTupleIdx (Tuple typs, op) i = do
    assert (i >= 0 && i < length typs) "tuple index out of range"
    o <- extractValue op [fromIntegral i]
    return (typs !! i, o)
    

valsEqual :: Value -> Value -> Instr Value
valsEqual (aTyp_, aOp) (bTyp_, bOp) = do
    aTyp <- getConcreteType aTyp_
    bTyp <- getConcreteType bTyp_
    assert (aTyp == bTyp) "types don't match"
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
            _         -> cmpErr "invalid comparison"
        

