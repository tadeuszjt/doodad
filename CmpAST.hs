{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module CmpAST where

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

import           LLVM.Context
import           LLVM.AST                   hiding (function, Module)
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.Global
import           LLVM.AST.IntegerPredicate
import qualified LLVM.AST.FloatingPointPredicate as F
import           LLVM.AST.Type              hiding (void, double)
import           LLVM.AST.Typed
import qualified LLVM.Internal.FFI.DataLayout as FFI
import           Foreign.Ptr
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified Lexer                      as L
import qualified AST                        as S
import           CmpFuncs
import           CmpValue
import           CmpMonad
import           CmpADT


compile :: Context -> Ptr FFI.DataLayout -> MyCmpState -> S.AST -> IO (Either CmpError ([Definition], MyCmpState))
compile context dataLayout state ast = do
    (res, _) <- runStateT (runModuleCmpT emptyModuleBuilder state cmp) (initCompileState context dataLayout)
    return $ fmap (\((_, defs), state') -> (defs, state')) res
    where
        cmp :: Module ()
        cmp = void $ function "main" [] VoidType $ \_ -> do
            getInstrCmp (mapM_ cmpTopStmt ast)


cmpPattern :: S.Pattern -> Value -> Instr Value
cmpPattern pattern val = case pattern of
    S.PatIgnore pos ->
        return (consBool True)

    S.PatLiteral expr -> do
        valsEqual val =<< cmpExpr expr

    S.PatIdent pos sym -> withPos pos $ do
        checkSymKeyUndefined sym KeyType
        v <- valLocal (valType val)
        valStore v val
        addSymObj sym KeyVal (ObjVal v)
        return (consBool True)

    S.PatTuple pos patterns -> withPos pos $ do
        Tuple ts <- getTupleType (valType val)
        cnds <- forM (zip patterns [0..]) $ \(pat, i) ->
            cmpPattern pat =<< valTupleIdx val i
        valAnd cnds

    S.PatTyped pos sym pattern -> withPos pos $ do
        res <- look sym KeyType
        case res of
            ObjType typ -> do
                match <- typesMatch typ (valType val)
                cmpPattern pattern val


cmpTopStmt :: S.Stmt -> Instr ()
cmpTopStmt stmt@(S.Set _ _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.Print _ _)      = cmpStmt stmt
cmpTopStmt stmt@(S.CallStmt _ _ _) = cmpStmt stmt
cmpTopStmt stmt@(S.Switch _ _ _)   = cmpStmt stmt
cmpTopStmt stmt@(S.Datadef _ _ _)  = cmpDataDef stmt

cmpTopStmt (S.Typedef pos symbol astTyp) = do
    checkUndefined symbol

    let typ    = fromASTType astTyp
    let typdef = Typedef symbol
    zero <- fmap cons (zeroOf typ)
    conc <- getConcreteType typ
    opTyp <- opTypeOf typ

    if isTuple typ then do
        name <- freshName (mkBSS symbol)
        addAction name (typedef name $ Just opTyp)
        addSymObj symbol KeyType $ ObjType (Named name typ)
        addSymObjReq symbol KeyType name

    else
        addSymObj symbol KeyType (ObjType typ)

cmpTopStmt (S.Assign pos pattern expr) = do
    val <- cmpExpr expr
    conc <- getConcreteType (valType val)
    assignPattern pattern conc val

    where
        assignPattern ::  S.Pattern -> ValType -> Value -> Instr ()
        assignPattern (S.PatTuple p patterns) conc val
            | isTuple conc = withPos p $ do
                len <- valLen val
                assert (fromIntegral (length patterns) == len) "incorrect tuple length"
                forM_ (zip patterns [0..]) $ \(pattern, i) ->
                    assignPattern pattern conc =<< valTupleIdx val i
            | length patterns == 1 = withPos p $ do
                assignPattern (head patterns) conc val

        assignPattern (S.PatArray p patterns) conc val 
            | isArray conc = withPos p $ do
                forM_ (zip patterns [0..]) $ \(pattern, i) ->
                    assignPattern pattern conc =<< valArrayConstIdx val i

--        assignPattern (S.PatTyped p sym pattern) conc val = withPos p $ do
--            ObjDataCons _ (Tuple ts) datEn <- look sym KeyDataCons
--
--            en <- valLoad =<< valTupleIdx val 0
--            Val Bool eq <- valsEqual en $ consInt (valType en) (fromIntegral datEn)
--            cont <- fresh
--            exc <- freshName "pattern_fail"
--            condBr eq cont exc
--            emitBlockStart exc
--            trap
--            emitBlockStart cont
--
--            tup <- valLocal (Tuple $ tail ts)
--            dat <- valCast (Tuple ts) val
--            len <- valLen tup
--            forM_ [0..len-1] $ \i ->
--                valTupleSet tup (fromIntegral i) =<< valTupleIdx dat (fromIntegral i+1)
--            assignPattern (S.PatTuple p patterns) (valType tup) tup
--
        assignPattern (S.PatIdent p symbol) conc val = withPos p $ do
            checkUndefined symbol
            name <- freshName (mkBSS symbol)
            (new, ext) <- valGlobal name (valType val)
            valStore new val

            addSymObj symbol KeyVal (ObjVal new)
            addSymObjReq symbol KeyVal name
            addAction name (emitDefn ext)
            addDeclared name
            addExported name

        assignPattern pat _ _ =
            withPos (S.pos pat) (cmpErr "invalid assignment pattern")

cmpTopStmt (S.Func pos symbol params mretty block) = withPos pos $ do
    let paramTyps    = map (fromASTType . S.paramType) params
    let paramSymbols = map S.paramName params
    let retTyp       = maybe Void fromASTType mretty
    paramTyps' <- mapM skipAnnos paramTyps
    publicFunction symbol (zip paramSymbols paramTyps') retTyp $ \args ->
        mapM_ cmpStmt block
        

cmpStmt :: S.Stmt -> Instr ()
cmpStmt (S.CallStmt pos symbol args) = void $ cmpExpr (S.Call pos symbol args)

cmpStmt (S.Assign pos pattern expr) = do
    Val Bool cnd <- cmpPattern pattern =<< cmpExpr expr
    if_ cnd (return ()) (trap)

cmpStmt (S.Set pos index expr) = withPos pos $ do
    val <- cmpExpr expr
    idx <- idxPtr index
    assert (valType val == valType idx) "type mismatch"
    valStore idx val
    where
        idxPtr :: S.Index -> Instr Value
        idxPtr (S.IndIdent p symbol) = withPos p $ do
            ObjVal loc@(Ptr _ _) <- look symbol KeyVal
            return loc

        idxPtr (S.IndArray p ind exp) = withPos p $ do
            arr <- idxPtr ind
            idx <- cmpExpr exp
            valArrayIdx arr idx

        idxPtr (S.IndTuple p ind i) = withPos p $ do
            tup <- idxPtr ind
            valTupleIdx tup i

cmpStmt (S.Switch pos expr [])    = void (cmpExpr expr)
cmpStmt (S.Switch pos expr cases) = withPos pos $ do
    pushSymTab
    val <- cmpExpr expr
    casesM <- forM cases $ \(casePattern, caseStmt) -> do
        let cmpCnd = do
            pushSymTab
            Val Bool cnd <- cmpPattern casePattern val
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
cmpExpr (S.Int pos i)   = return $ Val I64 (int64 i)
cmpExpr (S.Float pos f) = return $ Val F64 (double f)
cmpExpr (S.Bool pos b)  = return $ Val Bool (bit $ if b then 1 else 0)
cmpExpr (S.Char pos c)  = return $ Val Char (int32 $ fromIntegral $ fromEnum c)

cmpExpr (S.String pos s) = do
    name <- freshName "string"
    str <- globalStringPtr s name
    addExported name
    return $ Val String (cons str)

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
        cmpTypeFn :: ValType -> [Value] -> Instr Value
        cmpTypeFn typ args = do
            conc <- getConcreteType typ
            concs <- mapM getConcreteType (map valType args)
            case concs of
                []                     -> Val typ <$> cons <$> zeroOf conc
                [t] | conc == t        -> return $ (head args) { valType = typ }
                ts  | conc == Tuple ts -> do
                    tup <- valLocal typ
                    forM_ (zip args [0..]) $ \(a, i) -> valTupleSet tup i a
                    return tup
                _ -> cmpErr "cannot construct type from arguments"

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
    tup <- valLocal $ Tuple (map valType vals)
    forM_ (zip vals [0..]) $ \(val, i) -> valTupleSet tup i val
    return tup

cmpExpr (S.ArrayIndex pos arrExpr idxExpr) = do
    arr <- cmpExpr arrExpr
    idx <- cmpExpr idxExpr
    valArrayIdx arr idx

cmpExpr (S.TupleIndex pos tupExpr i) = do
    tup <- cmpExpr tupExpr
    typ <- getTupleType (valType tup)
    valTupleIdx (tup { valType = typ }) i

cmpExpr (S.TupleMember pos tupExpr member) = do
    tup <- cmpExpr tupExpr
    Just idx <- indexAnno member tup
    return idx
    where
        indexAnno :: String -> Value -> Instr (Maybe Value)
        indexAnno str val = case valType val of
            Named _ t   -> indexAnno str (val { valType = t } )
            AnnoTyp s t -> do
                if s == str then do
                    return $ Just (val { valType = t })
                else
                    indexAnno str (val { valType = t })

            Typedef sym -> do
                res <- look sym KeyType
                case res of
                    ObjType t -> indexAnno str (val{ valType = t })
            
            Tuple ts -> do
                res <- forM (zip ts [0..]) $ \(t, i) -> do
                   idx@(Ptr _ _) <- valTupleIdx val i
                   indexAnno str (idx { valType = t })
                case find isJust res of
                    Nothing -> return Nothing
                    Just r  -> return r

            _ -> return Nothing


cmpExpr (S.Infix pos S.EqEq exprA exprB) = withPos pos $ do
    valA <- cmpExpr exprA
    valB <- cmpExpr exprB
    valsEqual valA valB
cmpExpr (S.Infix pos operator exprA exprB) = do
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
                invalid
