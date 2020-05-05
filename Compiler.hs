{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler where

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


data CompileState
    = CompileState
        { curRetType :: ValType
        }
    deriving (Show, Eq)

initCompileState
    = CompileState
        { curRetType = Void
        }


type Compile    = State CompileState
type MyCmpState = CmpState SymEntry
type Instr      = InstrCmpT SymEntry Compile
type Module     = ModuleCmpT SymEntry Compile


type Value = (ValType, Operand)
type Func  = ([ValType], ValType, Operand)


data SymEntry
    = SymVal Value
    | SymFunc [Func]
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
    | ArrayPtr Int ValType
    | ArrayVal Int ValType
    | Tuple [ValType]
    deriving (Show, Eq)


isVal, isFunc :: SymEntry -> Bool
isVal (SymVal _)   = True
isVal _            = False
isFunc (SymFunc _) = True
isFunc _           = False


isInt   I32            = True
isInt   I64            = True
isInt   _              = False
isFloat F32            = True
isFloat F64            = True
isFloat _              = False
isFirst x
    | isInt x          = True
    | isFloat x        = True
isFirst Bool           = True
isFirst Char           = True
isFirst String         = True
isFirst (ArrayVal _ _) = True
isFirst _              = False
isArray (ArrayVal _ _) = True
isArray (ArrayPtr _ _) = True
isArray _              = False
isTuple (Tuple _)      = True
isTuple _              = False


fromASTType :: S.Type -> ValType
fromASTType typ = case typ of
    S.TBool      -> Bool
    S.TI32       -> I32
    S.TI64       -> I64
    S.TF32       -> F32
    S.TF64       -> F64
    S.TChar      -> Char
    S.TString    -> String
    S.TArray n t -> ArrayVal n (fromASTType t)
    S.TTuple ts  -> Tuple (map fromASTType ts)


-- globals will be pointers!
-- cmpExpr gaurantees correct type
-- all Values will be correct
-- symtab entries will not be correct
opTypeOf :: ValType -> Type
opTypeOf typ = case typ of
    Void       -> VoidType
    I32          -> i32
    I64          -> i64
    F32          -> FloatingPointType DoubleFP
    F64          -> FloatingPointType HalfFP
    Bool         -> i1
    Char         -> i32
    Tuple typs   -> StructureType False (map opTypeOf typs)
    ArrayPtr n t -> ptr $ ArrayType (fromIntegral n) (opTypeOf t)
    ArrayVal n t -> ArrayType (fromIntegral n) (opTypeOf t)
    String       -> ptr i8

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




compile :: MyCmpState -> S.AST -> Either CmpError ([Definition], MyCmpState) 
compile state ast =
    let (res, _) = runState (runModuleCmpT emptyModuleBuilder state cmp) initCompileState in
    fmap (\((_, defs), state') -> (defs, state')) res
    where
        cmp :: Module ()
        cmp =
            void $ function "main" [] VoidType $ \_ ->
                getInstrCmp (mapM_ cmpTopStmt ast)


valArrayToPtr :: Value -> Instr Value
valArrayToPtr (ArrayVal n t, arr) = do
    loc <- alloca (typeOf arr) Nothing 0
    store loc 0 arr
    return (ArrayPtr n t, loc)

valArrayIdx :: Value -> Value -> Instr Value
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
valsEqual pos (aTyp, aOp) (bTyp, bOp) = do
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
--            ArrayVal n t -> do
--                cnd <- alloca i1 Nothing 0
--                store cnd 0 (bit 1)
--                a <- valArrayPtr (aTyp aOp)
--                b <- valArrayPtr (bTyp bOp)
--                for (int64 $ fromIntegral n) $ \i -> do
--                    pea <- gep a [int64 0, i]
--                    peb <- gep a [int64 0, i]
--                    eb <- load pa 0
--                    b <- load pb 0
--                    c <- opEquality t a b
--                    store cnd 0 =<< and c =<< load cnd 0
--                load cnd 0
            Tuple ts -> do
                cnds <- forM (zip ts [0..]) $ \(t, i) -> do
                    a <- extractValue aOp [i]
                    b <- extractValue bOp [i]
                    opEquality t a b
                cnd <- alloca i1 Nothing 0
                forM_ cnds (\c -> store cnd 0 =<< and c =<< load cnd 0)
                load cnd 0
            _         -> cmpErr pos "invalid comparison"
        


cmpTopStmt :: S.Stmt -> Instr ()
cmpTopStmt stmt@(S.Set _ _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.Print _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.CallStmt _ _ _) = cmpStmt stmt
cmpTopStmt stmt@(S.Switch _ _ _)   = cmpStmt stmt

cmpTopStmt (S.Assign pos pattern expr) = do
    assignPattern pos pattern =<< cmpExpr expr
    where
        assignPattern :: TextPos -> S.Pattern -> Value -> Instr ()
        assignPattern _   (S.PatIgnore _) _ =
            return ()

        assignPattern _ (S.PatArray pos ps) val@(ArrayVal n t, op) = do
            unless (length ps == n) (cmpErr pos "incorrect array length")
            forM_ (zip ps [0..]) $ \(p, i) ->
                assignPattern pos p =<< valArrayConstIdx val i

        assignPattern _ (S.PatArray pos ps) val@(ArrayPtr n t, op) = do
            unless (length ps == n) (cmpErr pos "incorrect array length")
            forM_ (zip ps [0..]) $ \(p, i) ->
                assignPattern pos p =<< valArrayConstIdx val i

        assignPattern _ (S.PatTuple pos ps) val@(Tuple ts, op) = do
            unless (length ps == length ts) (cmpErr pos "incorrect tuple length")
            forM_ (zip ps [0..]) $ \(p, i) ->
                assignPattern pos p =<< valTupleIdx pos val i

        assignPattern pos (S.PatIdent _ symbol) (typ, op) = do
            checkUndefined pos symbol
            name <- freshName (mkBSS symbol)
            unless (isFirst typ) (cmpErr pos "isn't an expression")
            let opType = typeOf op
            loc <- global name opType (zeroOf typ)
            store loc 0 op
            addExtern symbol (globalDef name opType Nothing)
            addSymbol symbol $ SymVal (typ, loc)
            addDeclared symbol
            addExported symbol

        assignPattern pos _ _ =
            cmpErr pos "invalid pattern"
            
cmpTopStmt (S.Extern pos symbol params retty) = do
    checkUndefined pos symbol
    let name = mkName symbol

    let retType = maybe Void fromASTType retty
    unless (isFirst retType) (cmpErr pos "return isn't expression type")
    let retOpType = opTypeOf retType

    (paramTypes, paramOpTypes) <- fmap unzip $ forM params $ \(S.Param pos name typ) -> do
        let paramType = fromASTType typ
        unless (isFirst paramType) (cmpErr pos "param isn't an expression")
        return (paramType, opTypeOf paramType)
    
    op <- ensureExtern symbol paramOpTypes retOpType False
    addSymbol symbol $ SymFunc [(paramTypes, retType, op)]

cmpTopStmt (S.Func pos symbol params retty stmts) = do
        checkUndefined pos symbol
        name <- freshName (mkBSS symbol)
        let Name nameStr = name
        
        let retType = maybe Void fromASTType retty
        unless (retType == Void || isFirst retType) $
            cmpErr pos "return type isn't an expression"

        paramTypes <- forM params $ \(S.Param pos paramName paramType) -> do
            let typ = fromASTType paramType
            unless (isFirst typ) (cmpErr pos "param type isn't an expression")
            return typ

        let retOpType    = opTypeOf retType
        let paramSymbols = map S.paramName params
        let paramNames   = map mkName paramSymbols
        let paramNames'  = map (ParameterName . mkBSS) paramSymbols
        let paramOpTypes = map opTypeOf paramTypes

        let ext    = funcDef name (zip paramOpTypes paramNames) retOpType []
        let opType = FunctionType retOpType paramOpTypes False
        let op     = cons $ C.GlobalReference (ptr opType) name

        addDeclared symbol
        addExported symbol
        addExtern symbol ext
        addSymbol symbol $ SymFunc [(paramTypes, retType, op)]

        void $ InstrCmpT $ IRBuilderT . lift $ function name (zip paramOpTypes paramNames') retOpType $
            \args -> (flip named) nameStr $ getInstrCmp $ do
                pushSymTab
                curRetType <- lift (gets curRetType)
                lift $ modify $ \s -> s { curRetType = retType }

                forM_ (zip4 paramSymbols paramTypes paramOpTypes args) $ \(sym, typ, opType, arg)-> do
                    loc <- alloca opType Nothing 0
                    store loc 0 arg
                    addSymbol sym $ SymVal (typ, loc)
                    
                mapM_ cmpStmt stmts
                popSymTab
                lift $ modify $ \s -> s { curRetType = curRetType }
        

cmpStmt :: S.Stmt -> Instr ()
cmpStmt (S.Print pos exprs) = do
    prints =<< mapM cmpExpr exprs
    void (putchar '\n')
    where
        prints :: [Value] -> Instr ()
        prints []     = return ()
        prints [val]  = print val
        prints (v:vs) = print v >> printf ", " [] >> prints vs

        print :: Value -> Instr ()
        print val@(typ, op)
            | isArray typ = do
                vals <- forM [0..(valArrayLen val)-1] $ \i ->
                    valArrayConstIdx val i
                putchar '['
                prints vals
                void (putchar ']')

        print val@(typ, op) = case typ of
            I32  -> void (printf "%d" [op])
            I64  -> void (printf "%ld" [op])
            F32  -> void (printf "f" [op])
            F64  -> void (printf "%f" [op])
            Char -> void (putchar' op)
            String -> void (printf "%s" [op])

            Bool -> do
                str <- globalStringPtr "true\0false" =<< fresh
                idx <- select op (int64 0) (int64 5)
                ptr <- gep (cons str) [idx]
                void (printf "%s" [ptr])

            Tuple typs -> do
                ops <- forM [0..length typs-1] $ \i ->
                    extractValue op [toEnum i]

                putchar '('
                prints (zip typs ops)
                void (putchar ')')

            t -> cmpErr pos ("can't print type: " ++ show typ)

cmpStmt (S.Assign pos pattern expr) = do
    assignPattern pos pattern =<< cmpExpr expr
    where
        assignPattern :: TextPos -> S.Pattern -> Value -> Instr ()
        assignPattern _   (S.PatIgnore _) _ = return ()
        assignPattern _ (S.PatArray pos ps) val@(ArrayPtr n t, op) = do
            unless (length ps == n) (cmpErr pos "incorrect array length")
            forM_ (zip ps [0..]) $ \(p, i) ->
                assignPattern pos p =<< valArrayConstIdx val i

        assignPattern pos (S.PatTuple _ ps)  (Tuple ts, op) = do
            unless (length ps == length ts) (cmpErr pos "incorrect tuple length")
            forM_ (zip3 ps ts [0..]) $ \(p, t, i) -> do
                o <- extractValue op [fromIntegral i]
                assignPattern pos p (t, o)

        assignPattern pos (S.PatIdent _ symbol) (typ, op) = do
            checkUndefined pos symbol
            name <- freshName (mkBSS symbol)
            unless (isFirst typ) (cmpErr pos "isn't an expression")
            loc <- alloca (typeOf op) Nothing 0
            store loc 0 op
            addSymbol symbol $ SymVal (typ, loc)

        assignPattern pos _ _ =
            cmpErr pos "invalid pattern"
        
cmpStmt (S.Set pos symbol expr) = do
    entry <- look pos symbol
    unless (isVal entry) (cmpErr pos "symbol isn't a value")
    let SymVal (symType, loc) = entry
    (exprType, op) <- cmpExpr expr
    unless (symType == exprType) (cmpErr pos "types don't match")

    void $ case symType of
        Bool -> store loc 0 op
        I64  -> store loc 0 op

cmpStmt (S.CallStmt pos symbol args) = do
    entry <- look pos symbol
    unless (isFunc entry) (cmpErr pos "symbol isn't func")
    let SymFunc [(paramTypes, _, op)] = entry

    (argTypes, argOps) <- fmap unzip (mapM cmpExpr args)
    unless (argTypes == paramTypes) (cmpErr pos "arg types don't match param types")
    void $ call op (map (,[]) argOps)

cmpStmt (S.Return pos expr) = do
    curRetType <- lift (gets curRetType) 
    if isNothing expr then do
        unless (curRetType == Void) (cmpErr pos "must return expression")
        retVoid
    else do
        (typ, op) <- cmpExpr (fromJust expr)
        unless (curRetType == typ) $ cmpErr pos ("incorrect type: " ++ show typ)
        ret op
    void block

cmpStmt (S.Block pos stmts) = do
    pushSymTab
    mapM_ cmpStmt stmts
    popSymTab

cmpStmt (S.If pos expr block els) = do
    (typ, cnd) <- cmpExpr expr
    unless (typ == Bool) (cmpErr pos "expression isn't boolean")
    true  <- freshName (mkBSS "if_true")
    false <- freshName (mkBSS "if_false")
    exit  <- fresh
    condBr cnd true false
    emitBlockStart true
    cmpStmt block
    br exit
    emitBlockStart false
    when (isJust els) $ cmpStmt (fromJust els)
    br exit
    emitBlockStart exit

cmpStmt (S.Switch pos expr []) = return ()
cmpStmt (S.Switch pos expr cases) = do
    exit      <- fresh
    cndNames  <- replicateM (length cases) (freshName "case")
    stmtNames <- replicateM (length cases) (freshName "case_stmt")
    let nextNames = tail cndNames ++ [exit]
    let (caseExprs, stmts) = unzip cases

    br (head cndNames)
    pushSymTab

    forM_ (zip5 caseExprs cndNames stmtNames nextNames stmts) $
        \(caseExpr, cndName, stmtName, nextName, stmt) -> do
            emitBlockStart cndName
            if isJust caseExpr then do
                val <- cmpExpr expr
                cas <- cmpExpr (fromJust caseExpr)
                (_, cnd) <- valsEqual pos val cas
                condBr cnd stmtName nextName
            else
                br stmtName
            emitBlockStart stmtName
            cmpStmt stmt
            br exit

    popSymTab
    br exit
    emitBlockStart exit


cmpExpr :: S.Expr -> Instr (ValType, Operand)
cmpExpr expr = case expr of
    S.Int pos i   -> return (I64, int64 i)
    S.Float pos f -> return (F64, double f)
    S.Bool pos b  -> return (Bool, bit $ if b then 1 else 0)
    S.Char pos c  -> return (Char, int32 $ fromIntegral $ fromEnum c)

    S.Ident pos symbol -> do
        entry <- look pos symbol
        unless (isVal entry) $ cmpErr pos (symbol ++ "isn't an expression")
        let SymVal (typ, loc) = entry
        op <- load loc 0
        return (typ, op)
    
    S.String pos str -> do
        ptr <- globalStringPtr str =<< fresh
        return (String, cons ptr)

    S.Call pos symbol args -> do
        entry <- look pos symbol
        unless (isFunc entry) $ cmpErr pos (symbol ++ " isn't a function")
        let SymFunc [(paramTypes, retType, op)] = entry

        (argTypes, argOps) <- fmap unzip (mapM cmpExpr args)
        unless (argTypes == paramTypes) (cmpErr pos "arg types don't match param types")
        r <- call op $ map (,[]) argOps
        return (retType, r)
    
    S.Constructor pos newASTType expr -> do
        let newType = fromASTType newASTType
        unless (isFirst newType) $ cmpErr pos ("isn't expression type")
        (typ, op) <- cmpExpr expr
        unless (isFirst newType) $ cmpErr pos ("isn't an expression")
        newOp <- case (newType, typ) of
            (I32, I64)  -> trunc op i32
            (I32, Char) -> return op
            (Char, I32) -> return op
            (Char, I64) -> trunc op i32

        return (newType, newOp)

    S.Array pos exprs -> do
        let num = length exprs
        unless (num > 0) (cmpErr pos "can't deduce array type")
        (types, ops) <- fmap unzip (mapM cmpExpr exprs)
        let elemType = head types
        unless (isFirst elemType) (cmpErr pos "element type isn't expression")
        unless (all (== elemType) types) (cmpErr pos "element types don't match")
        name <- fresh
        let typ = ArrayVal num elemType
        loc <- alloca (opTypeOf typ) Nothing 0
        forM_ (zip ops [0..]) $ \(o, i) -> do
            ptr <- gep loc [int64 0, int64 i]
            store ptr 0 o

        op <- load loc 0
        return (typ, op)

    S.Tuple pos [arg] -> cmpExpr arg
    S.Tuple pos args  -> do
        (typs, ops) <- fmap unzip (mapM cmpExpr args)
        let tupOpType = StructureType False (map typeOf ops)
        loc <- alloca tupOpType Nothing 0

        forM_ (zip3 typs ops [0..]) $ \(typ, op, i) -> do
            loc' <- load loc 0
            agg <- insertValue loc' op [i]
            store loc 0 agg

        op <- load loc 0
        return (Tuple typs, op)
        
    S.TupleIndex pos tup idx -> do
        (typ, op) <- cmpExpr tup
        unless (isTuple typ) (cmpErr pos "expr isn't tuple")
        let Tuple typs = typ
        unless (idx >= 0 && idx < length typs) (cmpErr pos "tuple index out of range")
        o <- extractValue op [fromIntegral idx]
        return (typs !! idx, o)

    S.ArrayIndex pos arr idx -> do
        arrVal@(arrTyp, arrOp) <- cmpExpr arr
        idxVal@(idxTyp, idxOp) <- cmpExpr idx
        unless (isArray arrTyp) (cmpErr pos "expression isn't an array")
        unless (isInt idxTyp) (cmpErr pos "index isn't an integer")
        valArrayIdx arrVal idxVal

    S.Len pos expr -> do
        (typ, op) <- cmpExpr expr
        unless (isArray typ) (cmpErr pos "not an array")
        let len = valArrayLen (typ, op)
        return (I64, int64 $ fromIntegral len)

    S.Prefix pos operator expr -> do
        let invalid = cmpErr pos "invalid prefix"
        (typ, op) <- cmpExpr expr
        case typ of
            I64 -> case operator of
                S.Plus  -> return (I64, op)
                S.Minus -> fmap (I64,) $ sub (int64 0) op
                _       -> invalid 
            _ -> invalid 

    S.Infix pos S.EqEq expr1 expr2 -> do
        val1 <- cmpExpr expr1
        val2 <- cmpExpr expr2
        valsEqual pos val1 val2
    S.Infix pos operator expr1 expr2 -> do
        let invalid = cmpErr pos "invalid infix"
        let rt      = \typ ins -> fmap (typ,) ins
        val1@(typ1, op1) <- cmpExpr expr1
        val2@(typ2, op2) <- cmpExpr expr2

        case (typ1, typ2) of
            (Bool, Bool) ->
                case operator of
                    S.AndAnd -> rt Bool (and op1 op2)
                    S.OrOr   -> rt Bool (or op1 op2)
                    _        -> invalid

            (I64, I64) ->
                case operator of
                    S.Plus   -> rt I64 (add op1 op2)
                    S.Minus  -> rt I64 (sub op1 op2)
                    S.Times  -> rt I64 (mul op1 op2)
                    S.Divide -> rt I64 (sdiv op1 op2)
                    S.Mod    -> rt I64 (srem op1 op2)
                    S.LT     -> rt Bool (icmp SLT op1 op2)
                    S.GT     -> rt Bool (icmp SGT op1 op2)
                    S.LTEq   -> rt Bool (icmp SLE op1 op2)
                    S.GTEq   -> rt Bool (icmp SGT op1 op2)
                    _        -> invalid

            (Char, Char) ->
                case operator of
                    S.LT     -> rt Bool (icmp SLT op1 op2)
                    _        -> invalid

            _-> invalid

    _ -> error (show expr)
