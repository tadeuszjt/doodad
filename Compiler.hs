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
import qualified LLVM.AST.FloatingPointPredicate as F
import           LLVM.AST.Type              hiding (void, double)
import           LLVM.AST.Typed
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified Lexer                      as L
import qualified AST                        as S
import           CmpBuilder
import           CmpVal
import           Cmp


compile :: MyCmpState -> S.AST -> Either CmpError ([Definition], MyCmpState) 
compile state ast = do
    let (res, _) = runState (runModuleCmpT emptyModuleBuilder state cmp) initCompileState
    fmap (\((_, defs), state') -> (defs, state')) res
    where
        cmp :: Module ()
        cmp =
            void $ function "main" [] VoidType $ \_ ->
                getInstrCmp (mapM_ cmpTopStmt ast)


cmpTopStmt :: S.Stmt -> Instr ()
cmpTopStmt stmt@(S.Set _ _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.Print _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.CallStmt _ _ _) = cmpStmt stmt
cmpTopStmt stmt@(S.Switch _ _ _)   = cmpStmt stmt

cmpTopStmt (S.Assign pos pattern expr) =
    assignPattern pattern =<< cmpExpr expr
    where
        assignPattern ::  S.Pattern -> Value -> Instr ()
        assignPattern (S.PatTuple p patterns) val@(Tuple ts, _) = withPos p $ do
            assert (length patterns == length ts) "incorrect tuple length"
            forM_ (zip patterns [0..]) $ \(pattern, idx) ->
                assignPattern pattern =<< valTupleIdx val idx

        assignPattern (S.PatArray p patterns) val@(typ, _)
            | isArray typ = withPos p $ do
                forM_ (zip patterns [0..]) $ \(pattern, idx) ->
                    assignPattern pattern =<< valArrayConstIdx val idx

        assignPattern (S.PatIdent p symbol) val = withPos p $ do
            checkUndefined symbol
            name <- freshName (mkBSS symbol)
            (typ, loc, ext) <- valGlobalClone name val

            addSymObj symbol KeyVal $ ObjVal (typ, loc)
            addSymObjReq symbol KeyVal name
            addDef name ext
            addDeclared name
            addExported name

        assignPattern pat _ = withPos (S.pos pat) (cmpErr "invalid assignment pattern")


cmpStmt :: S.Stmt -> Instr ()
cmpStmt (S.CallStmt pos symbol args) = void $ cmpExpr (S.Call pos symbol args)
cmpStmt (S.Print pos exprs) =
    prints =<< mapM cmpExpr exprs
    where
        prints :: [Value] -> Instr ()
        prints []     = return ()
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs


cmpExpr :: S.Expr -> Instr Value
cmpExpr (S.Int pos i)   = return (I64, int64 i)
cmpExpr (S.Float pos f) = return (F64, double f)
cmpExpr (S.Bool pos b)  = return (Bool, bit $ if b then 1 else 0)
cmpExpr (S.Char pos c)  = return (Char, int32 $ fromIntegral $ fromEnum c)

cmpExpr (S.Call pos symbol args) = do
    (argTypes, argOps)   <- fmap unzip (mapM cmpExpr args)
    ObjFunc retType fnOp <- look symbol (KeyFunc argTypes)
    op <- call fnOp $ map (,[]) argOps
    return (retType, op)

cmpExpr (S.Ident pos symbol) = do
    ObjVal val <- look symbol KeyVal
    valLoad val

cmpExpr (S.Array pos exprs) = do
    let num = length exprs
    unless (num > 0) (cmpErr "can't deduce array type")

    vals <- mapM (valFlatten) =<< mapM cmpExpr exprs
    let (typs, _) = unzip vals
    let elemType = head typs
    unless (all (== elemType) typs) (cmpErr "element types don't match")

    let typ = ArrayPtr num elemType
    opType <- opTypeOf (ArrayVal num elemType)
    loc <- alloca opType Nothing 0
    forM_ (zip vals [0..]) $ \(val, i) ->
        valArraySet (typ, loc) (I64, int64 i) val

    return (typ, loc)

cmpExpr (S.Tuple pos exprs) = do
    vals <- mapM valFlatten =<< mapM cmpExpr exprs
    let (typs, ops) = unzip vals

    let typ = Tuple typs
    opTyp <- opTypeOf typ
    loc <- alloca opTyp Nothing 0
    forM_ (zip ops [0..]) $ \(op, i) -> do
        ptr <- gep loc [int32 0, int32 i]
        store ptr 0 op

    tup <- load loc 0
    return (typ, tup)


cmpExpr (S.Infix pos operator a b) = do
    va <- cmpExpr a
    vb <- cmpExpr b
    cmpInfix operator va vb
    where
        cmpInfix ::  S.Op -> Value -> Value -> Instr Value
        cmpInfix S.EqEq va vb =
            valsEqual va vb
        cmpInfix operator (typA_, opA) (typB_, opB) = do
            typA <- getConcreteType typA_
            typB <- getConcreteType typB_

            let invalid = cmpErr "invalid infix"
            let rt      = \typ ins -> fmap (typ,) ins

            case (typA, typB) of
                (Bool, Bool) ->
                    case operator of
                        S.AndAnd -> rt Bool (and opA opB)
                        S.OrOr   -> rt Bool (or opA opB)
                        _        -> invalid

                (I64, I64) ->
                    case operator of
                        S.Plus   -> rt I64 (add opA opB)
                        S.Minus  -> rt I64 (sub opA opB)
                        S.Times  -> rt I64 (mul opA opB)
                        S.Divide -> rt I64 (sdiv opA opB)
                        S.Mod    -> rt I64 (srem opA opB)
                        S.LT     -> rt Bool (icmp SLT opA opB)
                        S.GT     -> rt Bool (icmp SGT opA opB)
                        S.LTEq   -> rt Bool (icmp SLE opA opB)
                        S.GTEq   -> rt Bool (icmp SGT opA opB)
                        _        -> invalid

                (I32, I32) ->
                    case operator of
                        S.Plus   -> rt I32 (add opA opB)
                        S.Minus  -> rt I32 (sub opA opB)
                        S.Times  -> rt I32 (mul opA opB)
                        S.Divide -> rt I32 (sdiv opA opB)
                        S.Mod    -> rt I32 (srem opA opB)
                        S.LT     -> rt Bool (icmp SLT opA opB)
                        S.GT     -> rt Bool (icmp SGT opA opB)
                        S.LTEq   -> rt Bool (icmp SLE opA opB)
                        S.GTEq   -> rt Bool (icmp SGT opA opB)
                        _        -> invalid

                (F64, F64) ->
                    case operator of
                        S.Plus   -> rt F64 (fadd opA opB)
                        S.Minus  -> rt F64 (fsub opA opB)
                        S.Times  -> rt F64 (fmul opA opB)
                        S.Divide -> rt F64 (fdiv opA opB)
                        S.Mod    -> rt F64 (frem opA opB)
                        S.LT     -> rt Bool (fcmp F.OLT opA opB)
                        S.GT     -> rt Bool (fcmp F.OGT opA opB)
                        S.LTEq   -> rt Bool (fcmp F.OLE opA opB)
                        S.GTEq   -> rt Bool (fcmp F.OGE opA opB)
                        _        -> invalid
                (Char, Char) ->
                    case operator of
                        S.LT     -> rt Bool (icmp SLT opA opB)
                        _        -> invalid

                _-> invalid




        --cmpStmt (S.Block pos stmts)          = pushSymTab >> mapM_ cmpStmt stmts >> popSymTab
        --
        --cmpStmt (S.Assign pos pattern expr) = do
        --    (typ, op) <- cmpExpr expr
        --    t <- getConcreteType pos typ
        --    assignPattern pos pattern (t, op)
        --    where
        --        assignPattern :: TextPos -> S.Pattern -> Value -> Instr ()
        --        assignPattern _   (S.PatIgnore _) _ = return ()
        --        assignPattern _ (S.PatArray pos ps) val@(ArrayPtr n t, op) = do
        --            unless (length ps == n) (cmpErr "incorrect array length")
        --            forM_ (zip ps [0..]) $ \(p, i) ->
        --                assignPattern pos p =<< valArrayConstIdx val i
        --
        --        assignPattern pos (S.PatTuple _ ps)  (Tuple ts, op) = do
        --            unless (length ps == length ts) (cmpErr "incorrect tuple length")
        --            forM_ (zip3 ps ts [0..]) $ \(p, t, i) -> do
        --                o <- extractValue op [fromIntegral i]
        --                assignPattern pos p (t, o)
        --
        --        assignPattern pos (S.PatIdent _ symbol) (typ, op) = do
        --            checkUndefined pos symbol
        --            name <- freshName (mkBSS symbol)
        --            unless (isExpr typ) (cmpErr "isn't an expression")
        --            loc <- alloca (typeOf op) Nothing 0
        --            store loc 0 op
        --            addSymObj symbol KeyVal $ ObjVal (typ, loc)
        --
        --        assignPattern pos _ _ =
        --            cmpErr "invalid pattern"
        --    S.String pos str -> do
        --        ptr <- globalStringPtr str =<< fresh
        --        return (String, cons ptr)
        --    
        --    S.Constructor pos newASTType expr -> do
        --        newType <- fromASTType pos newASTType
        --        unless (isExpr newType) $ cmpErr ("isn't expression type")
        --        (typ, op) <- cmpExpr expr
        --        unless (isExpr newType) $ cmpErr ("isn't an expression")
        --        newOp <- case (newType, typ) of
        --            (I64, I64)  -> return op
        --            (I32, I64)  -> trunc op i32
        --            (I32, Char) -> return op
        --            (Char, I32) -> return op
        --            (Char, I64) -> trunc op i32
        --
        --        return (newType, newOp)
        --
        --
        --    S.Tuple pos [arg] -> cmpExpr arg
        --    S.Tuple pos args  -> do
        --        (typs, ops) <- fmap unzip (mapM cmpExpr args)
        --        let tupOpType = StructureType False (map typeOf ops)
        --        loc <- alloca tupOpType Nothing 0
        --
        --        forM_ (zip3 typs ops [0..]) $ \(typ, op, i) -> do
        --            loc' <- load loc 0
        --            agg <- insertValue loc' op [i]
        --            store loc 0 agg
        --
        --        op <- load loc 0
        --        return (Tuple typs, op)
        --        
        --    S.TupleIndex pos tup idx -> do
        --        (typ, op) <- cmpExpr tup
        --        unless (isTuple typ) (cmpErr "expr isn't tuple")
        --        let Tuple typs = typ
        --        unless (idx >= 0 && idx < length typs) (cmpErr "tuple index out of range")
        --        o <- extractValue op [fromIntegral idx]
        --        return (typs !! idx, o)
        --
        --    S.ArrayIndex pos arr idx -> do
        --        arrVal@(arrTyp, arrOp) <- cmpExpr arr
        --        idxVal@(idxTyp, idxOp) <- cmpExpr idx
        --        unless (isArray arrTyp) (cmpErr "expression isn't an array")
        --        unless (isInt idxTyp) (cmpErr "index isn't an integer")
        --        valArrayIdx arrVal idxVal
        --
        --    S.Len pos expr -> do
        --        (typ, op) <- cmpExpr expr
        --        unless (isArray typ) (cmpErr "not an array")
        --        let len = valArrayLen (typ, op)
        --        return (I64, int64 $ fromIntegral len)
        --
        --    S.Prefix pos operator expr -> do
        --        let invalid = cmpErr "invalid prefix"
        --        (typ, op) <- cmpExpr expr
        --        case typ of
        --            I64 -> case operator of
        --                S.Plus  -> return (I64, op)
        --                S.Minus -> fmap (I64,) $ sub (int64 0) op
        --                _       -> invalid 
        --            _ -> invalid 
        --
        --    S.Infix pos S.EqEq expr1 expr2 -> do
        --        val1 <- cmpExpr expr1
        --        val2 <- cmpExpr expr2
        --        valsEqual pos val1 val2
        --    S.Infix pos operator expr1 expr2 -> do
        --        let invalid = cmpErr "invalid infix"
        --        let rt      = \typ ins -> fmap (typ,) ins
        --        (typ1_, op1) <- cmpExpr expr1
        --        (typ2_, op2) <- cmpExpr expr2
        --        typ1 <- getConcreteType pos typ1_
        --        typ2 <- getConcreteType pos typ2_
        --
        --        case (typ1, typ2) of
        --            (Bool, Bool) ->
        --                case operator of
        --                    S.AndAnd -> rt Bool (and op1 op2)
        --                    S.OrOr   -> rt Bool (or op1 op2)
        --                    _        -> invalid
        --
        --            (I64, I64) ->
        --                case operator of
        --                    S.Plus   -> rt I64 (add op1 op2)
        --                    S.Minus  -> rt I64 (sub op1 op2)
        --                    S.Times  -> rt I64 (mul op1 op2)
        --                    S.Divide -> rt I64 (sdiv op1 op2)
        --                    S.Mod    -> rt I64 (srem op1 op2)
        --                    S.LT     -> rt Bool (icmp SLT op1 op2)
        --                    S.GT     -> rt Bool (icmp SGT op1 op2)
        --                    S.LTEq   -> rt Bool (icmp SLE op1 op2)
        --                    S.GTEq   -> rt Bool (icmp SGT op1 op2)
        --                    _        -> invalid
        --
        --            (I32, I32) ->
        --                case operator of
        --                    S.Plus   -> rt I32 (add op1 op2)
        --                    S.Minus  -> rt I32 (sub op1 op2)
        --                    S.Times  -> rt I32 (mul op1 op2)
        --                    S.Divide -> rt I32 (sdiv op1 op2)
        --                    S.Mod    -> rt I32 (srem op1 op2)
        --                    S.LT     -> rt Bool (icmp SLT op1 op2)
        --                    S.GT     -> rt Bool (icmp SGT op1 op2)
        --                    S.LTEq   -> rt Bool (icmp SLE op1 op2)
        --                    S.GTEq   -> rt Bool (icmp SGT op1 op2)
        --                    _        -> invalid
        --
        --            (F64, F64) ->
        --                case operator of
        --                    S.Plus   -> rt F64 (fadd op1 op2)
        --                    S.Minus  -> rt F64 (fsub op1 op2)
        --                    S.Times  -> rt F64 (fmul op1 op2)
        --                    S.Divide -> rt F64 (fdiv op1 op2)
        --                    S.Mod    -> rt F64 (frem op1 op2)
        --                    S.LT     -> rt Bool (fcmp F.OLT op1 op2)
        --                    S.GT     -> rt Bool (fcmp F.OGT op1 op2)
        --                    S.LTEq   -> rt Bool (fcmp F.OLE op1 op2)
        --                    S.GTEq   -> rt Bool (fcmp F.OGE op1 op2)
        --                    _        -> invalid
        --            (Char, Char) ->
        --                case operator of
        --                    S.LT     -> rt Bool (icmp SLT op1 op2)
        --                    _        -> invalid
        --
        --            _-> invalid
        --
        --    _ -> error (show expr)
        --
        --cmpTopStmt (S.Typedef pos symbol typ) = do
        --    checkUndefined pos symbol
        --    t <- fromASTType pos typ
        --    addSymObj symbol KeyType (ObjType t)
        --    typeZeroConstructor symbol t
        --    typeConcreteConstructor symbol t
        --
        --    where
        --        typeZeroConstructor :: String -> ValType -> Instr ()
        --        typeZeroConstructor symbol t = do
        --            retType <- getConcreteType pos t
        --            retOpType <- opTypeOf retType
        --            name <- freshName (mkBSS symbol)
        --            let opType = FunctionType retOpType [] False
        --            let op     = cons $ C.GlobalReference (ptr opType) name
        --            addSymObj symbol (KeyFunc []) (ObjFunc t op)
        --            void $ InstrCmpT $ IRBuilderT . lift $ function name [] retOpType $ \_ ->
        --                getInstrCmp $ ret $ cons (zeroOf retType)
        --
        --        typeConcreteConstructor :: String -> ValType -> Instr ()
        --        typeConcreteConstructor symbol t = do
        --            retType <- getConcreteType pos t
        --            retOpType <- opTypeOf retType
        --            name <- freshName (mkBSS symbol)
        --            let opType = FunctionType retOpType [retOpType] False
        --            let op     = cons $ C.GlobalReference (ptr opType) name
        --            addSymObj symbol (KeyFunc [t]) (ObjFunc retType op)
        --            void $ InstrCmpT $ IRBuilderT . lift $ function name [(retOpType, NoParameterName)] retOpType $ \[arg] ->
        --                getInstrCmp (ret arg)
        --

                    
        --cmpTopStmt (S.Extern pos symbol params retty) = do
        --    checkUndefined pos symbol
        --    let name = mkName symbol
        --
        --    retType <- maybe (return Void) (fromASTType pos) retty
        --    unless (isExpr retType) (cmpErr "return isn't expression type")
        --    retOpType <- opTypeOf retType
        --
        --    (paramTypes, paramOpTypes) <- fmap unzip $ forM params $ \(S.Param pos name typ) -> do
        --        paramType <- fromASTType pos typ
        --        unless (isExpr paramType) (cmpErr "param isn't an expression")
        --        opTyp <- opTypeOf paramType
        --        return (paramType, opTyp)
        --    
        --    op <- ensureExtern (mkName symbol) paramOpTypes retOpType False
        --    addSymObj symbol (KeyFunc paramTypes) (ObjFunc retType op)
        --
        --cmpTopStmt (S.Func pos symbol params retty stmts) = do
        --    name <- freshName (mkBSS symbol)
        --    let Name nameStr = name
        --    
        --    retType <- maybe (return Void) (fromASTType pos) retty
        --    unless (retType == Void || isExpr retType) $
        --        cmpErr "return type isn't an expression"
        --    retOpType <- opTypeOf retType
        --
        --    paramTypes <- forM params $ \(S.Param pos paramName paramType) -> do
        --        typ <- fromASTType pos paramType
        --        unless (isExpr typ) (cmpErr "param type isn't an expression")
        --        return typ
        --    paramOpTypes <- mapM opTypeOf paramTypes
        --    
        --    let key = KeyFunc paramTypes
        --    entry <- lookupSymKey symbol key
        --    when (isJust entry) $ cmpErr (symbol ++ "already defined")
        --
        --    let paramSymbols = map S.paramName params
        --    let paramNames   = map mkName paramSymbols
        --    let paramNames'  = map (ParameterName . mkBSS) paramSymbols
        --
        --    let ext    = funcDef name (zip paramOpTypes paramNames) retOpType []
        --    let opType = FunctionType retOpType paramOpTypes False
        --    let op     = cons $ C.GlobalReference (ptr opType) name
        --
        --    addDeclared name
        --    addExported name
        --    addExtern name paramOpTypes retOpType False
        --    addSymObj symbol key (ObjFunc retType op)
        --    addSymObjReq symbol (KeyFunc paramTypes) name
        --
        --    void $ InstrCmpT $ IRBuilderT . lift $ function name (zip paramOpTypes paramNames') retOpType $
        --        \args -> getInstrCmp $ do
        --            pushSymTab
        --            curRetType <- lift (gets curRetType)
        --            lift $ modify $ \s -> s { curRetType = retType }
        --
        --            forM_ (zip4 paramSymbols paramTypes paramOpTypes args) $ \(sym, typ, opType, arg)-> do
        --                loc <- alloca opType Nothing 0
        --                store loc 0 arg
        --                addSymObj symbol KeyVal $ ObjVal (typ, loc)
        --                
        --            mapM_ cmpStmt stmts
        --            popSymTab
        --            lift $ modify $ \s -> s { curRetType = curRetType }
        --    
        --

        --cmpStmt (S.If pos expr block els) = do
        --    (typ, cnd) <- cmpExpr expr
        --    unless (typ == Bool) (cmpErr "expression isn't boolean")
        --    if_ cnd (cmpStmt block) $
        --        when (isJust els) $ cmpStmt (fromJust els)
        --
        --cmpStmt (S.Return pos expr) = do
        --    curRetType <- lift (gets curRetType) 
        --    if isNothing expr then do
        --        unless (curRetType == Void) (cmpErr "must return expression")
        --        retVoid
        --    else do
        --        val@(typ, op) <- cmpExpr (fromJust expr)
        --        tm <- typesMatch pos typ curRetType
        --        unless tm $ cmpErr ("incorrect type: " ++ show typ)
        --        (_, flat) <- flattenVal pos val
        --        ret flat
        --    void block
        --
        --cmpStmt (S.Switch pos expr []) = return ()
        --cmpStmt (S.Switch pos expr cases) = do
        --    exit      <- fresh
        --    cndNames  <- replicateM (length cases) (freshName "case")
        --    stmtNames <- replicateM (length cases) (freshName "case_stmt")
        --    let nextNames = tail cndNames ++ [exit]
        --    let (caseExprs, stmts) = unzip cases
        --
        --    br (head cndNames)
        --    pushSymTab
        --
        --    forM_ (zip5 caseExprs cndNames stmtNames nextNames stmts) $
        --        \(caseExpr, cndName, stmtName, nextName, stmt) -> do
        --            emitBlockStart cndName
        --            if isJust caseExpr then do
        --                val <- cmpExpr expr
        --                cas <- cmpExpr (fromJust caseExpr)
        --                (_, cnd) <- valsEqual pos val cas
        --                condBr cnd stmtName nextName
        --            else
        --                br stmtName
        --            emitBlockStart stmtName
        --            cmpStmt stmt
        --            br exit
        --
        --    popSymTab
        --    br exit
        --    emitBlockStart exit
        --
