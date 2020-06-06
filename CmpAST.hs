{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module CmpAST where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.List                  hiding (and, or)
import           Prelude                    hiding (EQ, and, or)

import           LLVM.Context
import           LLVM.AST                   hiding (Type, function, Module)
import           LLVM.AST.Type              hiding (Type, void, double) 
import qualified LLVM.AST.Type              as LL  (Type) 
import           LLVM.AST.Global
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.IntegerPredicate
import qualified LLVM.AST.FloatingPointPredicate as F
import qualified LLVM.Internal.FFI.DataLayout as FFI
import           Foreign.Ptr
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified AST                        as S
import           Type
import           CmpFuncs
import           CmpMonad
import           CmpADT
import           Value
import           Table
import           Print
import           Error


compile :: Context -> Ptr FFI.DataLayout -> MyCmpState -> Map.Map String S.Expr -> S.AST -> IO (Either CmpError ([Definition], MyCmpState))
compile context dataLayout state exprMap ast = do
    (res, _) <- runStateT (runModuleCmpT emptyModuleBuilder state cmp) (initCompileState context dataLayout exprMap)
    return $ fmap (\((_, defs), state') -> (defs, state')) res
    where
        cmp :: Module ()
        cmp = void $ function "main" [] VoidType $ \_ -> do
            getInstrCmp (mapM_ cmpTopStmt ast)


cmpPattern :: Bool -> S.Pattern -> Value -> Instr Value
cmpPattern isGlobal pattern val = case pattern of
    S.PatIgnore pos   -> return (valBool True)
    S.PatLiteral cons -> valsEqual val =<< cmpExpr (S.Cons cons)

    S.PatIdent pos sym -> withPos pos $ do
        if isGlobal then do
            name <- freshName (mkBSS sym)
            (v, ext) <- valGlobal name (valType val)

            typ <- realTypeOf (valType val)
            case typ of
                Table _ _ -> valTableStore v val
                _         -> valStore v val

            addSymObj sym KeyVal (ObjVal v)
            addSymObjReq sym KeyVal name
            addAction name (emitDefn ext)
            addDeclared name
            addExported name
        else do
            v <- valLocal (valType val)
            valStore v val
            addSymObj sym KeyVal (ObjVal v)
        return (valBool True)

    S.PatTuple pos patterns -> withPos pos $ do
        Tuple nm ts <- realTypeOf (valType val)
        assert (length patterns == length ts) "incorrect tuple length"
        cnds <- forM (zip patterns [0..]) $ \(pat, i) ->
            cmpPattern isGlobal pat =<< valTupleIdx i val
        valAnd cnds

    S.PatTyped pos sym pattern -> withPos pos $ do
        res <- lookupSymKey sym KeyType
        case res of
            Just (ObjType typ) -> do
                checkTypesMatch (Typedef sym) (valType val)
                cmpPattern isGlobal pattern val
            Nothing -> do
                res <- lookupSymKey sym KeyDataCons
                case res of
                    Just (ObjDataCons d t@(Tuple _ ts) e) -> do
                        ensureTypeDeps d
                        checkTypesMatch d (valType val)
                        let val' = val { valType = t }
                        en <- valTupleIdx 0 val'
                        eq <- valsEqual en $ valInt (valType en) (fromIntegral e)
                        if_ (valOp eq) (return ()) trap
                        let ts' = tail ts
                        tup <- valLocal (Tuple Nothing ts')
                        forM_ (zip ts' [0..]) $ \(_, i) ->
                            valTupleSet tup i =<< valTupleIdx (i+1) val'
                        cmpPattern isGlobal pattern tup
                        

cmpTopStmt :: S.Stmt -> Instr ()
cmpTopStmt stmt@(S.Set _ _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.Print _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.CallStmt _ _ _) = cmpStmt stmt
cmpTopStmt stmt@(S.Switch _ _ _)   = cmpStmt stmt
cmpTopStmt stmt@(S.While _ _ _)    = cmpStmt stmt
cmpTopStmt stmt@(S.Block _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.Extern _ _ _ _) = cmpStmt stmt
cmpTopStmt stmt@(S.Datadef _ _ _)  = cmpDataDef stmt

cmpTopStmt (S.Typedef pos symbol typ) = do
    if isTuple typ then do
        name <- freshName (mkBSS symbol)
        let Tuple Nothing ts = typ
        let typ' = Tuple (Just name) ts
        opTyp <- opTypeOf typ
        addAction name $ typedef name (Just opTyp)
        addSymObj symbol KeyType (ObjType typ')
        addSymObjReq symbol KeyType name
    else
        addSymObj symbol KeyType (ObjType typ)

cmpTopStmt (S.Assign pos pattern expr) = do
    Val Bool matched <- cmpPattern True pattern =<< cmpExpr expr
    if_ matched (return ()) trap

cmpTopStmt (S.Func pos symbol params mretty block) = withPos pos $ do
    name <- freshName (mkBSS symbol)
    
    let paramTypes   = map S.paramType params
    let paramSymbols = map S.paramName params
    let retTyp       = maybe Void id mretty
    let paramNames   = map mkName paramSymbols
    let paramFnNames = map (ParameterName . mkBSS) paramSymbols

    paramOpTypes <- mapM opTypeOf paramTypes
    retOpType    <- opTypeOf retTyp

    oldRetTyp <- getCurRetTyp
    setCurRetTyp retTyp

    op <- InstrCmpT $ IRBuilderT . lift $ function name (zip paramOpTypes paramFnNames) retOpType $ \a ->
        getInstrCmp $ do
            forM_ (zip3 paramSymbols paramTypes a) $ \(sym, typ, op) -> do
                arg <- valLocal typ
                valStore arg (Val typ op)
                addSymObj sym KeyVal (ObjVal arg)

            mapM_ cmpStmt block

            hasTerm <- hasTerminator
            unless hasTerm $
                if retTyp == Void
                then retVoid
                else ret . cons =<< zeroOf retTyp

    setCurRetTyp oldRetTyp

    addDeclared name
    addExported name
    addSymObj symbol (KeyFunc paramTypes) (ObjFunc retTyp op)
    addSymObjReq symbol (KeyFunc paramTypes) name
    addAction name $ emitDefn $ funcDef name (zip paramOpTypes paramNames) retOpType []
    return ()


cmpStmt :: S.Stmt -> Instr ()
cmpStmt (S.CallStmt pos symbol args) = void $ cmpExpr (S.Call pos symbol args)
cmpStmt (S.Block pos stmts) = mapM_ cmpStmt stmts

cmpStmt (S.Extern pos sym params mretty) = do
    let name         = mkName sym
    let paramTyps    = map S.paramType params
    let paramSymbols = map S.paramName params
    let retty        = maybe Void id mretty
    let paramNames   = map mkName paramSymbols
    paramTyps' <- mapM opTypeOf paramTyps
    retty' <- opTypeOf retty

    let def = funcDef name (zip paramTyps' paramNames) retty' []
    addAction name (emitDefn def)
    let op = ConstantOperand $ C.GlobalReference (ptr $ FunctionType retty' paramTyps' False) name
    addSymObj sym (KeyFunc paramTyps) (ObjFunc retty op)
    addSymObjReq sym (KeyFunc paramTyps) name

cmpStmt (S.Assign pos pattern expr) = do
    Val Bool cnd <- cmpPattern False pattern =<< cmpExpr expr
    if_ cnd (return ()) (trap)

cmpStmt (S.Return pos mexpr) = withPos pos $ do
    retTyp <- getCurRetTyp
    if isNothing mexpr then do
        assert (retTyp == Void) "must return expression"
        retVoid
    else do
        val <- valLoad =<< cmpExpr (fromJust mexpr)
        checkTypesMatch retTyp (valType val)
        ret (valOp val)
    return ()

cmpStmt (S.Set pos index expr) = withPos pos $ do
    val <- cmpExpr expr
    idx <- idxPtr index
    checkTypesMatch (valType val) (valType idx)
    typ <- realTypeOf (valType idx)
    case typ of
        Table _ _ -> valTableKill idx >> valTableStore idx val
        _         -> valStore idx val
    where
        idxPtr :: S.Index -> Instr Value
        idxPtr (S.IndIdent p symbol) = withPos p $ do
            ObjVal loc@(Ptr _ _) <- look symbol KeyVal
            return loc

        idxPtr (S.IndArray p ind exp) = withPos p $ do
            arr <- idxPtr ind
            valArrayIdx arr =<< cmpExpr exp

        idxPtr (S.IndTuple p ind i) = withPos p $
            valTupleIdx i =<< idxPtr ind

cmpStmt (S.Switch pos expr [])    = void (cmpExpr expr)
cmpStmt (S.Switch pos expr cases) = withPos pos $ do
    val <- cmpExpr expr
    casesM <- forM cases $ \(casePattern, caseStmt) -> do
        let cmpCnd = do
            Val Bool cnd <- cmpPattern False casePattern val
            return cnd
        let cStmt = do
            cmpStmt caseStmt
        return (cmpCnd, cStmt)

    switch_ casesM

cmpStmt (S.While pos expr stmts) = withPos pos $ do
    cond <- freshName "while_cnd"
    body <- freshName "while_body"
    exit <- fresh
    
    br cond
    emitBlockStart cond
    val <- valLoad =<< cmpExpr expr
    typ <- realTypeOf (valType val)
    assert (typ == Bool) "expression isn't bool"
    condBr (valOp val) body exit

    emitBlockStart body
    mapM_ cmpStmt stmts
    br cond

    emitBlockStart exit

cmpStmt (S.Print pos exprs) = withPos pos $
    prints =<< mapM cmpExpr exprs
    where
        prints :: [Value] -> Instr ()
        prints []     = return ()
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs


cmpExpr :: S.Expr -> Instr Value
cmpExpr (S.Cons c) = case c of
    S.Int pos i    -> return (valInt I64 i)
    S.Float pos f  -> return $ Val F64 (double f)
    S.Bool pos b   -> return $ Val Bool (bit $ if b then 1 else 0)
    S.Char pos c   -> return $ Val Char (int32 $ fromIntegral $ fromEnum c)
    S.String pos s -> do
        name <- freshName "string"
        str <- globalStringPtr s name
        addExported name
        return $ Val String (cons str)

cmpExpr (S.Table pos exprs) =
    cmpTableExpr =<< mapM (mapM cmpExpr) exprs

cmpExpr (S.Conv pos typ exprs) = withPos pos $ do
    vals <- mapM cmpExpr exprs
    let typs = map valType vals
    case typs of
        []  -> valLocal typ
        [t] -> cmpConv (head vals)
        ts  -> do
            tup@(Ptr _ _) <- valLocal (Tuple Nothing ts)
            forM_ (zip vals [0..]) $ \(val, i) -> valTupleSet tup i val
            cmpConv tup
    where
        cmpConv :: Value -> Instr Value
        cmpConv val
            | isIntegral typ = do
                conc <- concreteTypeOf (valType val)
                case (typ, conc) of
                    (I64, I64) -> return val
                    (I32, I64) -> fmap (Val I32) $ trunc (valOp val) i32
                    (I16, I64) -> fmap (Val I16) $ trunc (valOp val) (IntegerType 16)
                    (I8, I64)  -> fmap (Val I8) $ trunc (valOp val) i8


cmpExpr (S.Ident pos symbol) = do
    res <- look symbol KeyVal
    case res of
        ObjVal val  -> return val
        ObjInline f -> f []

cmpExpr (S.Array pos exprs) = do
    vals <- mapM cmpExpr exprs
    let n = fromIntegral (length vals)
    assert (n > 0) "can't deduce array type"
    let t = valType (head vals)
    arr <- valLocal (Array n t)
    forM_ (zip vals [0..]) $ \(val, i) ->
        valArraySet arr (Val I64 $ int64 i) val
    return arr

cmpExpr (S.Tuple pos exprs) = do
    vals <- mapM cmpExpr exprs
    tup <- valLocal $ Tuple Nothing (map valType vals)
    forM_ (zip vals [0..]) $ \(val, i) -> valTupleSet tup i val
    return tup

cmpExpr (S.Subscript pos expr index) = do
    val <- cmpExpr expr
    idx <- cmpExpr index
    typ <- realTypeOf (valType val)
    case typ of
        Array _ _ -> valArrayIdx val idx
        Table _ _ -> valTableIdx val idx
        _ -> cmpErr ("invalid array index for type: " ++ show (valType val))

cmpExpr (S.TupleIndex pos tupExpr i) = do
    tup <- cmpExpr tupExpr
    Tuple nm ts <- realTypeOf (valType tup)
    valTupleIdx i (tup { valType = Tuple nm ts })

cmpExpr (S.Len pos expr) = do
    val <- cmpExpr expr
    typ <- realTypeOf (valType val)
    case typ of
        Array n _   -> return (valInt I64 $ fromIntegral n)
        Table nm ts -> valTableLen val

cmpExpr (S.Append pos table elem) = withPos pos $ do
    tab <- cmpExpr table
    elm <- cmpExpr elem
    valTableAppend tab elm

cmpExpr (S.Member pos tupExpr symbol) = do
    tup <- cmpExpr tupExpr
    idx <- indexAnno symbol tup
    assert (isJust idx) ("undefined member " ++ symbol)
    return (fromJust idx)
    where
        indexAnno :: String -> Value -> Instr (Maybe Value)
        indexAnno str val = case valType val of
            Annotated s t
                | s == str  -> return $ Just (val { valType = t })
                | otherwise -> indexAnno str (val { valType = t })

            Typedef sym -> do
                res <- look sym KeyType
                case res of
                    ObjType t -> indexAnno str (val{ valType = t })
            
            Tuple nm ts -> do
                res <- forM (zip ts [0..]) $ \(t, i) -> do
                   idx@(Ptr _ _) <- valTupleIdx i val
                   indexAnno str (idx { valType = t })
                case find isJust res of
                    Nothing -> return Nothing
                    Just r  -> return r

            _ -> return Nothing

cmpExpr (S.Call pos symbol args) = withPos pos $ do
    vals <- mapM valLoad =<< mapM cmpExpr args
    mapM ensureTypeDeps (map valType vals)

    let key = KeyFunc (map valType vals)
    rfun <- lookupSymKey symbol key
    when (isJust rfun) (ensureSymDeps symbol key)
    rtyp <- lookupSymKey symbol KeyType 
    when (isJust rtyp) (ensureSymDeps symbol KeyType)

    case (rtyp, rfun) of
        (_, Just (ObjFunc typ op)) -> Val typ <$> call op [(o, []) | o <- map valOp vals]
        (_, Just (ObjInline f))    -> f vals
        (_, Just (ObjVal val))     -> return val
        (Just (ObjType t), _)      -> ensureTypeDeps t >> cmpTypeFn (Typedef symbol) vals
        _                          -> cmpErr ("no callable object exists for " ++ symbol)
    where
        cmpTypeFn :: Type -> [Value] -> Instr Value
        cmpTypeFn typ args = do
            conc <- concreteTypeOf typ
            argConcs <- mapM concreteTypeOf (map valType args)
            case argConcs of
                []             -> fmap (Val typ . cons) (zeroOf conc)
                [t] | typ == t -> return $ (head args) { valType = typ }
                ts             -> do 
                    assert (isTuple conc) "multiple arguments for non-tuple type constructor"
                    let Tuple _ tupTs = conc
                    assert (ts == tupTs) "arguments do not match tuple fields"
                    tup <- valLocal conc
                    forM_ (zip args [0..]) $ \(a, i) ->
                        valTupleSet tup i a
                    return $ tup { valType = typ }

cmpExpr (S.Infix pos S.EqEq exprA exprB) = withPos pos $ do
    valA <- cmpExpr exprA
    valB <- cmpExpr exprB
    valsEqual valA valB
cmpExpr (S.Infix pos operator exprA exprB) = withPos pos $ do
    valA <- valLoad =<< cmpExpr exprA
    valB <- valLoad =<< cmpExpr exprB
    cmpInfix operator valA valB
    where
        res typ = fmap (Val typ)

        invalid :: Instr a
        invalid = cmpErr "invalid infix"

        cmpInfix :: S.Op -> Value -> Value -> Instr Value
        cmpInfix operator (Val typA opA) (Val typB opB)
            | typA == typB && isIntegral typA = case operator of
                S.Plus   -> res typA (add opA opB)
                S.Minus  -> res typA (sub opA opB)
                S.Times  -> res typA (mul opA opB)
                S.Divide -> res typA (sdiv opA opB)
                S.Mod    -> res typA (srem opA opB)
                S.LT     -> res Bool (icmp SLT opA opB)
                S.GT     -> res Bool (icmp SGT opA opB)
                S.LTEq   -> res Bool (icmp SLE opA opB)
                S.GTEq   -> res Bool (icmp SGT opA opB)
                _        -> invalid

            | typA == typB && isFloat typA = case operator of
                S.Plus   -> res typA (fadd opA opB)
                S.Minus  -> res typA (fsub opA opB)
                S.Times  -> res typA (fmul opA opB)
                S.Divide -> res typA (fdiv opA opB)
                S.Mod    -> res typA (frem opA opB)
                S.LT     -> res Bool (fcmp F.OLT opA opB)
                S.GT     -> res Bool (fcmp F.OGT opA opB)
                S.LTEq   -> res Bool (fcmp F.OLE opA opB)
                S.GTEq   -> res Bool (fcmp F.OGE opA opB)
                _        -> invalid

            | typA == Bool && typB == Bool = case operator of
                S.AndAnd -> res Bool (and opA opB)
                S.OrOr   -> res Bool (or opA opB)
                _        -> invalid

            | otherwise =
                cmpErr ("invalid infix for " ++ show typA ++ " and " ++ show typB)
