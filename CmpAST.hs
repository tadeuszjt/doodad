{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module CmpAST where

import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Data.List                  hiding (and, or)
import           Prelude                    hiding (EQ, and, or)

import           LLVM.Context
import           LLVM.AST                   hiding (function, Module)
import           LLVM.AST.IntegerPredicate
import qualified LLVM.AST.FloatingPointPredicate as F
import qualified LLVM.Internal.FFI.DataLayout as FFI
import           Foreign.Ptr
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified AST                        as S
import           CmpFuncs
import           CmpValue
import           CmpMonad
import           CmpADT
import           CmpTable


compile :: Context -> Ptr FFI.DataLayout -> MyCmpState -> S.AST -> IO (Either CmpError ([Definition], MyCmpState))
compile context dataLayout state ast = do
    (res, _) <- runStateT (runModuleCmpT emptyModuleBuilder state cmp) (initCompileState context dataLayout)
    return $ fmap (\((_, defs), state') -> (defs, state')) res
    where
        cmp :: Module ()
        cmp = void $ function "main" [] VoidType $ \_ -> do
            getInstrCmp (mapM_ cmpTopStmt ast)


cmpPattern :: Bool -> S.Pattern -> Value -> Instr Value
cmpPattern isGlobal pattern val = case pattern of
    S.PatIgnore pos ->
        return (valBool True)

    S.PatLiteral expr ->
        valsEqual val =<< cmpExpr expr

    S.PatIdent pos sym -> withPos pos $ do
        checkSymKeyUndefined sym KeyType
        if isGlobal then do
            checkSymKeyUndefined sym KeyType
            name <- freshName (mkBSS sym)
            (v, ext) <- valGlobal name (valType val)
            valStore v val
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
        Tuple nm ts <- nakedTypeOf (valType val)
        cnds <- forM (zip patterns [0..]) $ \(pat, i) ->
            cmpPattern isGlobal pat =<< valTupleIdx i val
        valAnd cnds

    S.PatTyped pos sym pattern -> withPos pos $ do
        res <- look sym KeyType
        case res of
            ObjType typ -> do
                checkTypesMatch typ (valType val)
                cmpPattern isGlobal pattern val


cmpTopStmt :: S.Stmt -> Instr ()
cmpTopStmt stmt@(S.Set _ _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.Print _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.CallStmt _ _ _) = cmpStmt stmt
cmpTopStmt stmt@(S.Switch _ _ _)   = cmpStmt stmt
cmpTopStmt stmt@(S.Datadef _ _ _)  = cmpDataDef stmt

cmpTopStmt (S.Typedef pos symbol astTyp) = do
    checkUndefined symbol
    let typ = fromASTType astTyp
    opTyp <- opTypeOf typ
    if isTuple typ then do
        let Tuple Nothing ts = typ
        name <- freshName (mkBSS symbol)
        let typ' = Tuple (Just name) ts
        addAction name $ typedef name (Just opTyp)
        addSymObj symbol KeyType (ObjType typ')
        addSymObjReq symbol KeyType name
    else
        addSymObj symbol KeyType (ObjType typ)

cmpTopStmt (S.Assign pos pattern expr) = do
    Val Bool matched <- cmpPattern True pattern =<< cmpExpr expr
    if_ matched (return ()) trap

--cmpTopStmt (S.Func pos symbol patterns mretty block) = withPos pos $ do
--    let paramTyps    = map (fromASTType . S.paramType) params
--    let paramSymbols = map S.paramName params
--    let retTyp       = maybe Void fromASTType mretty
--    paramTyps' <- mapM skipAnnos paramTyps
--    publicFunction symbol (zip paramSymbols paramTyps') retTyp $ \args ->
--        mapM_ cmpStmt block


cmpStmt :: S.Stmt -> Instr ()
cmpStmt (S.CallStmt pos symbol args) =
    void $ cmpExpr (S.Call pos symbol args)

cmpStmt (S.Assign pos pattern expr) = do
    Val Bool cnd <- cmpPattern False pattern =<< cmpExpr expr
    if_ cnd (return ()) (trap)

cmpStmt (S.Set pos index expr) = withPos pos $ do
    val <- cmpExpr expr
    idx <- idxPtr index
    checkTypesMatch (valType val) (valType idx)
    valStore idx val
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
    pushSymTab
    val <- cmpExpr expr
    casesM <- forM cases $ \(casePattern, caseStmt) -> do
        let cmpCnd = do
            pushSymTab
            Val Bool cnd <- cmpPattern False casePattern val
            return cnd
        let cStmt = do
            cmpStmt caseStmt
            popSymTab
        return (cmpCnd, cStmt)

    switch_ casesM
    popSymTab

cmpStmt (S.Return pos mexpr) = withPos pos $ do
    retTyp <- getCurRetTyp
    if isNothing mexpr then do
        assert (retTyp == Void) "must return expression"
        retVoid
    else do
        val <- valLoad =<< cmpExpr (fromJust mexpr)
        assert (retTyp == valType val) "incorrect return type" 
        ret (valOp val)
    return ()

cmpStmt (S.Print pos exprs) = withPos pos $
    prints =<< mapM cmpExpr exprs
    where
        prints :: [Value] -> Instr ()
        prints []     = return ()
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs


cmpExpr :: S.Expr -> Instr Value
cmpExpr (S.Int pos i)       = return (valInt I64 i)
cmpExpr (S.Float pos f)     = return $ Val F64 (double f)
cmpExpr (S.Bool pos b)      = return $ Val Bool (bit $ if b then 1 else 0)
cmpExpr (S.Char pos c)      = return $ Val Char (int32 $ fromIntegral $ fromEnum c)
cmpExpr (S.Table pos exprs) = cmpTableExpr =<< mapM (mapM cmpExpr) exprs

cmpExpr (S.String pos s) = do
    name <- freshName "string"
    str <- globalStringPtr s name
    addExported name
    return $ Val String (cons str)

cmpExpr (S.Ident pos symbol) = do
    ObjVal val <- look symbol KeyVal
    ensureTypeDeps (valType val)
    return val

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

cmpExpr (S.ArrayIndex pos arrExpr idxExpr) = do
    arr <- cmpExpr arrExpr
    idx <- cmpExpr idxExpr
    valArrayIdx arr idx

cmpExpr (S.TupleIndex pos tupExpr i) = do
    tup <- cmpExpr tupExpr
    Tuple nm ts <- nakedTypeOf (valType tup)
    valTupleIdx i (tup { valType = Tuple nm ts })

cmpExpr (S.TupleMember pos tupExpr symbol) = do
    tup <- cmpExpr tupExpr
    idx <- indexAnno symbol tup
    assert (isJust idx) ("undefined member " ++ symbol)
    return (fromJust idx)
    where
        indexAnno :: String -> Value -> Instr (Maybe Value)
        indexAnno str val = case valType val of
            AnnoTyp s t
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
        --(Just (ObjType t), _)      -> ensureTypeDeps t >> cmpTypeFn (Typedef symbol) vals
        _                          -> cmpErr ("no callable object exists for " ++ symbol)
    where
--        cmpTypeFn :: ValType -> [Value] -> Instr Value
--        cmpTypeFn typ args = do
--            conc <- concreteTypeOf typ
--            concs <- mapM concreteTypeOf (map valType args)
--            case concs of
--                []                        -> Val typ <$> cons <$> zeroOf conc
--                [t] | conc == t           -> return $ (head args) { valType = typ }
--                ts  | conc == Tuple nm ts -> do
--                    tup <- valLocal typ
--                    forM_ (zip args [0..]) $ \(a, i) -> valTupleSet tup i a
--                    return tup
--                _ -> cmpErr "cannot construct type from arguments"
--
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
