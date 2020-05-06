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
type MyCmpState = CmpState [SymEntry]
type Instr      = InstrCmpT [SymEntry] Compile
type Module     = ModuleCmpT [SymEntry] Compile


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


data SymEntry
    = SymVal Value
    | SymFunc Func
    | SymType ValType
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


isVal, isFunc :: SymEntry -> Bool
isVal (SymVal _)   = True
isVal _            = False
isFunc (SymFunc _) = True
isFunc _           = False


isInt t                             = t `elem` [I32, I64]
isFloat t                           = t `elem` [F32, F64]
isFirst x
    | isInt x                       = True
    | isFloat x                     = True
    | x `elem` [Bool, Char, String] = True
isFirst (ArrayVal _ _)              = True
isFirst (Typedef _)                 = True
isFirst _                           = False
isArray (ArrayVal _ _)              = True
isArray (ArrayPtr _ _)              = True
isArray _                           = False
isTuple (Tuple _)                   = True
isTuple _                           = False


lookupType :: ValType -> Instr (Maybe ValType)
lookupType (Typedef symbol) = do
    entries <- lookupSymbol symbol
    case entries of
        Nothing -> return Nothing
        Just ss -> return (lookupTypes ss)
    where
        lookupTypes :: [SymEntry] -> Maybe ValType
        lookupTypes (SymType t:_) = Just t
        lookupTypes (_:ss)        = lookupTypes ss
        lookupTypes _             = Nothing
lookupType t = return (Just t)


getConcreteType :: TextPos -> ValType -> Instr ValType
getConcreteType pos typ = do
    t <- lookupType typ
    unless (isJust t) (cmpErr pos "isn't a type")
    case fromJust t of
        Typedef s -> getConcreteType pos (Typedef s)
        t         -> return t


typesMatch :: TextPos -> ValType -> ValType -> Instr Bool
typesMatch pos a b = do
    ca <- getConcreteType pos a
    cb <- getConcreteType pos b
    return (ca == cb)
    

lookupFunction :: String -> [ValType] -> Instr (Maybe Func) 
lookupFunction symbol params = do
    entries <- lookupSymbol symbol
    case entries of
        Nothing -> return Nothing
        Just ss -> return (lookupParams params ss)
    where
        lookupParams :: [ValType] -> [SymEntry] -> Maybe Func
        lookupParams params (SymFunc f@(ps, _, _):_)
            | params == ps = Just f
        lookupParams params (_:ss) = lookupParams params ss
        lookupParams _ _           = Nothing


addFunction :: String -> Func -> Instr ()
addFunction symbol fn = do
    entry <- lookupSymbol symbol
    addSymbol symbol $ case entry of
        Nothing -> [SymFunc fn]
        Just ss -> SymFunc fn:ss


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


opTypeOf :: ValType -> Instr Type
opTypeOf typ = do
    t <- getConcreteType (TextPos 0 0 0) typ 
    case t of
        Void         -> return (VoidType)
        I32          -> return (i32)
        I64          -> return (i64)
        F32          -> return (FloatingPointType DoubleFP)
        F64          -> return (FloatingPointType HalfFP)
        Bool         -> return (i1)
        Char         -> return (i32)
        Tuple typs   -> fmap (StructureType False) (mapM opTypeOf typs)
        ArrayPtr n t -> fmap (ptr . (ArrayType $ fromIntegral n)) (opTypeOf t)
        ArrayVal n t -> fmap (ArrayType $ fromIntegral n) (opTypeOf t)
        String       -> return (ptr i8)


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


flattenVal :: TextPos -> Value -> Instr Value
flattenVal pos val@(ArrayVal _ _, _)     = valArrayToPtr val
flattenVal pos val@(typ@(Typedef _), op) = getConcreteType pos typ >>= \t -> flattenVal pos (t, op)
flattenVal pos val                       = return val


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
        

