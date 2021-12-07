{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Compile where

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Fail hiding (fail)
import Control.Monad.Except hiding (void, fail)
import Foreign.Ptr

import LLVM.AST.Name hiding (Func)
import qualified LLVM.AST as LL
import qualified LLVM.AST.Type as LL
import qualified LLVM.AST.Constant as C
import qualified LLVM.Internal.FFI.DataLayout as FFI
import LLVM.AST.Global
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Context

import qualified AST as S
import qualified Flatten as F
import Monad
import Type
import Error
import Value
import State
import Print
import Funcs
import Table
import Tuple
import ADT
import Construct
import Typeof
import Trace

compileFlatState
    :: BoM s m
    => Context
    -> Ptr FFI.DataLayout
    -> Map.Map S.ModuleName CompileState
    -> F.FlattenState
    -> S.ModuleName
    -> m ([LL.Definition], CompileState)
compileFlatState ctx dl imports flatState modName = do
    res <- runBoMT (initCompileState ctx dl imports modName) (runModuleCmpT emptyModuleBuilder cmp)
    case res of
        Left err                 -> throwError err
        Right ((_, defs), state) -> return (defs, state)
    where
        cmp :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        cmp = void $ func "main" [] LL.VoidType $ \_ -> do
            forM_ (Map.toList $ F.typeDefs flatState) $ \(flat, (pos, typ)) ->
                withPos pos $ cmpTypeDef flat typ
            mapM_ cmpFuncHdr (F.funcDefs flatState)
            mapM_ cmpExternDef (F.externDefs flatState)
            mapM_ cmpVarDef (F.varDefs flatState)
            mapM_ cmpFuncDef (F.funcDefs flatState)


cmpTypeDef :: InsCmp CompileState m => String -> Type -> m ()
cmpTypeDef sym typ = trace "cmpTypeDef" $ do
    case typ of
        t | isADT t   -> adtTypeDef sym t
        t | isTuple t -> tupleTypeDef sym t
        t             -> do
            let typdef = Typedef (Sym sym)
            addObjWithCheck sym (KeyFunc []) (ObjConstructor typdef)
            addObjWithCheck sym (KeyFunc [t]) (ObjConstructor typdef)
            addObjWithCheck sym (KeyFunc [typdef]) (ObjConstructor typdef)
            addObjWithCheck sym KeyType (ObType t Nothing)
                    

cmpVarDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpVarDef (S.Assign pos (S.PatIdent p sym) expr) = trace "cmpVarDef" $ withPos pos $ do
    val <- valResolveExp =<< cmpExpr expr
    name <- myFresh sym

    let typ = valType val
    opTyp <- opTypeOf typ

    if isCons (valOp val)
    then do
        loc <- Ptr typ <$> global name opTyp (toCons $ valOp val)
        addObjWithCheck sym KeyVar (ObjVal loc)
    else do
        initialiser <- valZero typ
        loc <- Ptr typ <$> global name opTyp (toCons $ valOp initialiser)
        valStore loc val
        addObjWithCheck sym KeyVar (ObjVal loc)

    addSymKeyDec sym KeyVar name (DecVar opTyp)
    addDeclared name


cmpExternDef :: InsCmp CompileState m => S.Stmt -> m ()
cmpExternDef (S.Extern pos sym params retty) = trace "cmpExternDef" $ withPos pos $ do
    checkSymUndef sym 
    let name = LL.mkName sym
    let paramTypes = map S.paramType params

    pushSymTab
    paramOpTypes <- forM params $ \(S.Param p s t) -> do
        addObjWithCheck s KeyVar $ ObjVal (valBool False)
        opTypeOf t

    returnOpType <- opTypeOf retty
    popSymTab

    addSymKeyDec sym (KeyFunc paramTypes) name (DecExtern paramOpTypes returnOpType False)
    let op = fnOp name paramOpTypes returnOpType False
    addObj sym (KeyFunc paramTypes) (ObjExtern paramTypes retty op)


cmpFuncHdr :: InsCmp CompileState m => S.Stmt -> m ()
cmpFuncHdr (S.FuncDef pos "main" params retty blk) = trace "cmpFuncHdr" $ return ()
cmpFuncHdr (S.FuncDef pos sym params retty blk)    = trace "cmpFuncHdr" $ withPos pos $ do
    let paramTypes = map S.paramType params
    name <- myFresh sym
    paramOpTypes <- mapM opTypeOf paramTypes
    returnOpType <- opTypeOf retty
    let op = fnOp name paramOpTypes returnOpType False

    addObjWithCheck sym (KeyFunc paramTypes) (ObjFunc retty op) 
    addObj sym KeyVar $ ObjVal $ Val (Func paramTypes retty) op

    addSymKeyDec sym (KeyFunc paramTypes) name (DecFunc paramOpTypes returnOpType)
    addSymKeyDec sym KeyVar name (DecFunc paramOpTypes returnOpType)

    addDeclared name


cmpFuncDef :: (MonadFail m, Monad m, MonadIO m) => S.Stmt -> InstrCmpT CompileState m ()
cmpFuncDef (S.FuncDef pos "main" params retty blk) = trace "cmpFuncDef" $ withPos pos $ do
    assert (params == [])  "main cannot have parameters"
    assert (retty == Void) "main must return void"
    cmpStmt blk
cmpFuncDef (S.FuncDef pos sym params retty blk) = trace "cmpFuncDef" $ withPos pos $ do
    returnOpType <- opTypeOf retty
    paramOpTypes <- mapM (opTypeOf . S.paramType) params
    let paramTypes = map S.paramType params
    let paramNames = map (ParameterName . mkBSS . S.paramName) params
    let paramSyms  = map S.paramName params

    ObjFunc _ op <- look (Sym sym) (KeyFunc paramTypes)
    let LL.ConstantOperand (C.GlobalReference _ name) = op
    let Name nameStr = name

    pushSymTab
    curRetty <- gets curRetType
    modify $ \s -> s { curRetType = retty }
    void $ InstrCmpT . IRBuilderT . lift $ func name (zip paramOpTypes paramNames) returnOpType $ \paramOps -> do
        forM_ (zip3 paramTypes paramOps paramSyms) $ \(typ, op, sym) -> do
            loc <- valLocal typ
            valStore loc (Val typ op)
            addObjWithCheck sym KeyVar (ObjVal loc)

        cmpStmt blk
        hasTerm <- hasTerminator
        retty <- gets curRetType
        if hasTerm
        then return ()
        else if retty == Void
        then retVoid
        else unreachable

    modify $ \s -> s { curRetType = curRetty }
    popSymTab


cmpIndex:: InsCmp CompileState m => S.Index -> m Value
cmpIndex index = case index of
    S.IndIdent pos sym -> withPos pos $ do
        ObjVal val <- look (Sym sym) KeyVar
        return val
    S.IndArray pos ind idxExpr -> do
        idxVal <- valResolveExp =<< cmpExpr idxExpr
        assertBaseType isInt (valType idxVal)

        loc <- cmpIndex ind
        base <- baseTypeOf (valType loc)
        case base of
            Table [t] -> do
                row <- tableRow 0 loc
                ptr <- valPtrIdx row idxVal
                return ptr



cmpInfix :: InsCmp CompileState m => S.Op -> Value -> Value -> m Value
cmpInfix op valA valB = do
    resm <- lookm (Sym $ show op) $ KeyFunc [valType valA, valType valB]
    case resm of
        Just (ObjFunc Void op)   -> err "Operator function does not return a value."

        Just (ObjFunc retty op)  -> do
            opA <- valOp <$> valLoad valA
            opB <- valOp <$> valLoad valB
            Val retty <$> call op [(opA, []), (opB, [])]

        Nothing -> valsInfix op valA valB


cmpCondition :: InsCmp CompileState m => S.Condition -> m Value
cmpCondition cnd = trace "cmpCondition" $ do
    val <- case cnd of
        S.CondExpr expr      -> cmpExpr expr
        S.CondMatch pat expr -> cmpPattern pat =<< cmpExpr expr

    assertBaseType (== Bool) (valType val)
    return val


cmpPrint :: InsCmp CompileState m => S.Stmt -> m ()
cmpPrint (S.Print pos exprs) = trace "cmpPrint" $ withPos pos $ do
    prints =<< mapM valResolveExp =<< mapM cmpExpr exprs
    where
        prints :: InsCmp CompileState m => [Value] -> m ()
        prints []     = void $ printf "\n" []
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs


cmpStmt :: InsCmp CompileState m => S.Stmt -> m ()
cmpStmt stmt = trace "cmpStmt" $ case stmt of
    S.Print pos exprs -> cmpPrint stmt
    S.Block stmts -> pushSymTab >> mapM_ cmpStmt stmts >> popSymTab

    S.AppendStmt append -> case append of
        S.AppendTable pos (S.AppendIndex index) expr -> withPos pos $ do
            loc <- cmpIndex index
            val <- valResolveExp =<< cmpExpr expr
            tableAppend loc val

        S.AppendElem pos (S.AppendIndex index) expr -> withPos pos $ do
            loc <- cmpIndex index
            val <- valResolveExp =<< cmpExpr expr
            tableAppendElem loc val

        _ -> err $ show append

    S.CallStmt pos (S.IndIdent _ sym) exprs -> withPos pos $ do
        vals <- mapM (valLoad <=< valResolveExp <=< cmpExpr) exprs
        resm <- lookm (Sym sym) $ KeyFunc (map valType vals)
        op <- case resm of
            Just (ObjFunc _ op)     -> return op
            Just (ObjExtern _ _ op) -> return op
            Nothing                 -> do
                ObjVal fval <- look (Sym sym) KeyVar
                ftyp@(Func ts rt) <- assertBaseType isFunction (valType fval)
                assert (ts == map valType vals) ("Incorrect argument types for: " ++ show ftyp)
                valOp <$> valLoad fval

        void $ call op [(o, []) | o <- map valOp vals]

    S.CallStmt pos index exprs -> withPos pos $ do
        vals <- mapM (valLoad <=< valResolveExp <=< cmpExpr) exprs
        fval <- valLoad =<< cmpIndex index
        ftyp@(Func ts rt) <- assertBaseType isFunction (valType fval)
        assert (ts == map valType vals) ("Incorrect argument types for: " ++ show ftyp)
        void $ call (valOp fval) [(o, []) | o <- map valOp vals]

    S.Assign pos pat expr -> withPos pos $ trace ("assign " ++ show pat) $ do
        matched <- valLoad =<< cmpPattern pat =<< cmpExpr expr
        if_ (valOp matched) (return ()) (void trap) 

    S.Set pos ind expr -> withPos pos $ do
        val <- cmpExpr expr
        loc <- cmpIndex ind
        valStore loc val

    S.Return pos Nothing -> withPos pos $ do
        curRetty <- gets curRetType
        assert (curRetty == Void) "must return a value"
        retVoid
        emitBlockStart =<< fresh

    S.Return pos (Just expr) -> withPos pos $ do
        retty <- gets curRetType
        ret . valOp =<< valLoad =<< valAsType retty =<< cmpExpr expr
        emitBlockStart =<< fresh

    S.If pos cnd blk melse -> withPos pos $ do
        pushSymTab
        val <- valLoad =<< cmpCondition cnd
        assertBaseType (== Bool) (valType val)
        if_ (valOp val) (cmpStmt blk) $ maybe (return ()) cmpStmt melse
        popSymTab

    S.While pos cnd blk -> withPos pos $ do
        cond <- freshName "while_cond"
        body <- freshName "while_body"
        exit <- freshName "while_exit"

        pushSymTab
        br cond
        emitBlockStart cond
        val <- valLoad =<< cmpCondition cnd
        condBr (valOp val) body exit
        
        emitBlockStart body
        cmpStmt blk
        br cond
        emitBlockStart exit
        popSymTab

    S.For pos idxSym expr guardm blk -> withPos pos $ do
        val <- valResolveExp =<< cmpExpr expr
        assertBaseType isTable (valType val)
        len <- tableLen val

        idx <- valLocal I64
        valStore idx (valI64 0)

        pushSymTab
        addObjWithCheck idxSym KeyVar (ObjVal idx)

        cond <- freshName "for_cond"
        body <- freshName "for_body"
        exit <- freshName "for_exit"

        br cond
        emitBlockStart cond
        cnd <- valLocal Bool
        valStore cnd =<< valsInfix S.LT idx len
        when (isJust guardm) $ do
            valStore cnd =<< valsInfix S.AndAnd cnd =<< cmpExpr (fromJust guardm)

        cndOp <- valOp <$> valLoad cnd
        condBr cndOp body exit

        emitBlockStart body
        cmpStmt blk
        valStore idx =<< valsInfix S.Plus idx (valI64 1)
        br cond
        emitBlockStart exit

        popSymTab

    S.Switch pos expr cases -> withPos pos $ do
        val <- cmpExpr expr

        exitName <- freshName "switch_exit"
        trapName <- freshName "switch_trap"
        cndNames <- replicateM (length cases) (freshName "case")
        stmtNames <- replicateM (length cases) (freshName "case_stmt")
        let nextNames = cndNames ++ [trapName]

        br (head nextNames)
        forM_ (zip4 cases cndNames stmtNames (tail nextNames)) $
            \((pat, stmt), cndName, stmtName, nextName) -> do
                emitBlockStart cndName
                pushSymTab
                matched <- valLoad =<< cmpPattern pat val
                condBr (valOp matched) stmtName nextName
                emitBlockStart stmtName
                cmpStmt stmt
                popSymTab
                br exitName

        emitBlockStart trapName
        void trap 
        br exitName
        emitBlockStart exitName

    _ -> err "stmt"


-- must return Val unless local variable
cmpExpr :: InsCmp CompileState m =>  S.Expr -> m Value
cmpExpr expr = trace "cmpExpr" $ case expr of
    e | exprIsContextual e     -> return (Exp expr)
    S.Bool pos b               -> return (valBool b)
    S.Char pos c               -> return (valChar c)
    S.Conv pos typ []          -> valZero typ
    S.Conv pos typ [S.Null p]  -> withPos pos (adtNull typ)
    S.Conv pos typ exprs       -> withPos pos $ valConstruct typ =<< mapM cmpExpr exprs
    S.Copy pos expr            -> withPos pos $ valCopy =<< cmpExpr expr

    S.Infix pos op exprA exprB -> withPos pos $ do
        valA <- cmpExpr exprA
        valB <- cmpExpr exprB
        cmpInfix op valA valB

    S.Ident pos sym            -> withPos pos $ do
        obj <- look (Sym sym) KeyVar
        case obj of
            ObjVal loc             -> return loc
            ObjADTFieldCons adtTyp -> adtConstructField sym adtTyp []

    S.Prefix pos S.Not   expr  -> withPos pos $ valNot =<< cmpExpr expr
    S.Prefix pos S.Minus expr  -> withPos pos $ do
        val <- cmpExpr expr
        a <- valInt (valType val) 0
        valsInfix S.Minus a val
            
    S.String pos s -> do
        loc <- globalStringPtr s =<< myFresh "str"
        let pi8 = C.BitCast loc (LL.ptr LL.i8)
        let i64 = toCons $ int64 $ fromIntegral (length s)
        let stc = struct Nothing False [i64, i64, pi8]
        return $ Val (Table [Char]) stc

    S.Call pos expr exprs -> withPos pos $ trace "call" $ do
        vals <- mapM valLoad =<< mapM valResolveExp =<< mapM cmpExpr exprs

        (s, obj) <- case expr of
            S.Ident _ s                  -> fmap (s,) $ look (Sym s) $ KeyFunc (map valType vals)
            S.Member _ (S.Ident _ mod) s -> fmap (s,) $ look (SymQualified mod s) $ KeyFunc (map valType vals)
            _                            -> fmap ("",) $ fmap ObjVal $ valLoad =<< cmpExpr expr
        
        case obj of
            ObjFunc Void _         -> err "cannot use void function as expression"
            ObjExtern _ Void _     -> err "cannot use void function as expression"
            ObjConstructor typ     -> valConstruct typ vals
            ObjADTFieldCons adtTyp -> adtConstructField s adtTyp vals
            ObjFunc retty op       -> Val retty <$> call op [(o, []) | o <- map valOp vals]
            ObjExtern _ retty op   -> Val retty <$> call op [(o, []) | o <- map valOp vals]
            ObjVal (Val typ op)    -> do
                Func ts rt <- assertBaseType isFunction typ
                assert (map valType vals == ts) "Invalid argument types"
                Val rt <$> call op [(o, []) | o <- map valOp vals]

    S.Len pos expr -> withPos pos $ valLoad =<< do
        val <- valResolveExp =<< cmpExpr expr
        typ <- baseTypeOf (valType val)
        case typ of
            Table _ -> tableLen val
            _       -> err ("cannot take length of type " ++ show typ)

    S.Tuple pos [expr] -> withPos pos (cmpExpr expr)
    S.Tuple pos exprs -> withPos pos $ do
        vals <- mapM cmpExpr exprs
        assert (not $ any valIsContextual vals) "contextual 371"
        tup <- valLocal $ Tuple [ ("", valType v) | v <- vals ]
        zipWithM_ (tupleSet tup) [0..] vals
        valLoad tup

    S.Subscript pos aggExpr idxExpr -> withPos pos $ valLoad =<< do
        agg <- cmpExpr aggExpr
        idx <- valResolveExp =<< cmpExpr idxExpr

        idxType <- assertBaseType isInt (valType idx)
        aggType <- baseTypeOf (valType agg)

        case aggType of
            Table [t] -> tableGetElem agg idx

    S.Range pos expr mstart mend -> withPos pos $ do
        val <- cmpExpr expr
        assert (not $ valIsContextual val) "contextual 395"
        base <- baseTypeOf (valType val)
        case base of
            Table ts -> do
                start <- maybe (return $ valI64 0) (valResolveExp <=< cmpExpr) mstart
                valLoad =<< tableRange val start =<< maybe (tableLen val) (valResolveExp <=< cmpExpr) mend
    
    S.Table pos exprss -> withPos pos $ valLoad =<< do
        valss <- mapM (mapM (valResolveExp <=< cmpExpr)) exprss
        assert (length valss > 0) "Cannot infer type of table."
        let rowLen = length (head valss)
        assert (rowLen > 0) "Cannot infer type of table."

        rowTypes <- forM valss $ \vals -> do
            assert (length vals == rowLen) $ "Mismatched table row length of " ++ show (length vals)
            let typ = valType (head vals)
            mapM_ (checkTypesCompatible typ) (map valType vals)
            return typ

        rows <- forM (zip rowTypes [0..]) $ \(t, r) -> do
            mal <- valMalloc t (valI64 rowLen)
            forM_ [0..rowLen - 1] $ \i -> do
                ptr <- valPtrIdx mal (valI64 i) 
                valStore ptr ((valss !! r) !! i)
            return mal

        tab <- valLocal (Table rowTypes)
        tableSetLen tab (valI64 rowLen)
        tableSetCap tab (valI64 rowLen)
        zipWithM_ (tableSetRow tab) [0..] rows
        return tab

    S.Member pos exp sym -> withPos pos $ do
        tupleMember sym =<< cmpExpr exp 

    _ -> err ("invalid expression: " ++ show expr)


cmpPattern :: InsCmp CompileState m => S.Pattern -> Value -> m Value
cmpPattern pattern val = trace "cmpPattern" $ case pattern of
    S.PatIgnore pos -> return (valBool True)

    S.PatLiteral (S.Null pos) -> withPos pos $ trace "cmpPattern null" $ do
        ADT xs <- assertBaseType isADT (valType val)
        
        idx <- case [ i | (("", Void), i) <- zip xs [0..] ] of
            [i] -> return i
            _   -> err "ADT does not support a unique null field"

        valsInfix S.EqEq (valI64 idx) =<< adtEnum val

    S.PatLiteral expr -> cmpInfix S.EqEq val =<< cmpExpr expr

    S.PatGuarded pos pat expr -> withPos pos $ do
        match <- cmpPattern pat =<< valLoad val
        guard <- cmpExpr expr
        assertBaseType (== Bool) (valType guard)
        valsInfix S.AndAnd match guard

    S.PatIdent pos sym -> withPos pos $ trace ("cmpPattern " ++ show pattern) $ do
        val' <- valResolveExp val
        base <- baseTypeOf (valType val')

        if isADT base && (let ADT xs = base in sym `elem` map fst xs)
        then do
            let ADT xs = base
            let idx = fromJust $ elemIndex sym (map fst xs)
            let (s, t) = xs !! idx
            assert (t == Void) "Ident patterns only valid for null fields"
            valsInfix S.EqEq (valI64 idx) =<< adtEnum val'
        else do
            loc <- valLocal (valType val')
            valStore loc val'
            addObjWithCheck sym KeyVar (ObjVal loc)
            return (valBool True)

    S.PatTuple pos pats -> withPos pos $ do
        len <- tupleLength val
        assert (len == length pats) "tuple pattern length mismatch"

        bs <- forM (zip pats [0..]) $ \(p, i) ->
            cmpPattern p =<< valResolveExp =<< tupleIdx i val

        foldM (valsInfix S.AndAnd) (valBool True) bs

    S.PatArray pos pats -> withPos pos $ do
        base <- baseTypeOf (valType val)
        case base of
            Table ts -> do
                len   <- tableLen val
                lenEq <- valsInfix S.EqEq len (valI64 $ length pats)

                assert (length ts == 1) "patterns don't support multiple rows (yet)"
                bs <- forM (zip pats [0..]) $ \(p, i) ->
                    cmpPattern p =<< tableGetElem val (valI64 i)

                foldM (valsInfix S.AndAnd) (valBool True) (lenEq:bs)
            _ -> err "Invalid array pattern"

    S.PatSplit pos pat@(S.PatArray p pats) rest -> withPos pos $ do
        initMatched <- cmpPattern pat =<< tableRange val (valI64 0) (valI64 $ length pats)
        matched <- valLocal Bool
        if_ (valOp initMatched)
            (valStore matched =<< cmpPattern rest =<< tableRange val (valI64 $ length pats) =<< tableLen val)
            (valStore matched $ valBool False)
        valLoad matched

    S.PatSplit pos (S.PatLiteral (S.String p s)) rest -> withPos pos $ do
        let charPats = map (S.PatLiteral . S.Char p) s
        let arrPat   = S.PatArray p charPats
        cmpPattern (S.PatSplit pos arrPat rest) val

    S.PatSplitElem pos pat rest -> withPos pos $ do
        hasElem <- valsInfix S.LT (valI64 0) =<< tableLen val
        initMatched <- valLocal Bool
        if_ (valOp hasElem)
            (valStore initMatched =<< cmpPattern pat =<< tableGetElem val (valI64 0))
            (valStore initMatched $ valBool False)

        initMatchedVal <- valLoad initMatched
        matched <- valLocal Bool
        if_ (valOp initMatchedVal)
            (valStore matched =<< cmpPattern rest =<< tableRange val (valI64 1) =<< tableLen val)
            (valStore matched $ valBool False)

        valLoad matched

    S.PatTyped pos typ pats -> withPos pos $ do
        ADT xs <- assertBaseType isADT (valType val)

        let fieldMatched = \(s, t) -> (s == "" && t == typ) || Typedef (Sym s) == typ

        idx <- case [ i | (x, i) <- zip xs [0..], fieldMatched x ] of
            [i] -> return i
            []  -> err "Invalid ADT field identifier"
            _   -> err "Ambiguous ADT field identifier"

        enumMatched <- valsInfix S.EqEq (valI64 idx) =<< adtEnum val

        ptr <- valLocal $ ADT [xs !! idx]
        adtSetPi8 ptr =<< adtPi8 val
        drf <- adtDeref ptr

        case pats of
            []    -> err "Invalid pattern with no arguments."
            [pat] -> valsInfix S.AndAnd enumMatched =<< cmpPattern pat drf
            pats  -> do
                Tuple txs <- assertBaseType isTuple (valType drf)
                assert (length txs == length pats) "Invalid pattern"
                tupVals <- forM (zip txs [0..]) $ \(_, i) -> tupleIdx i drf
                bVals <- zipWithM cmpPattern pats tupVals
                foldM (valsInfix S.AndAnd) (valBool True) (enumMatched:bVals)
    
    _ -> err ("Cannot compile pattern: " ++ show pattern)



valAsType :: InsCmp CompileState m => Type -> Value -> m Value
valAsType typ val = trace "valAsType" $ case val of
    Exp (S.Int _ n)      -> valInt typ n
    Exp (S.Null _)       -> adtNull typ
    Exp (S.Table _ [[]]) -> assertBaseType isTable typ >> valZero typ

    Val _ _              -> do
        checkTypesCompatible typ (valType val)
        return $ val { valType = typ }

    Ptr _ _              -> do
        checkTypesCompatible typ (valType val)
        return $ val { valType = typ }

    Exp (S.Tuple _ es)   -> do
        Tuple xs <- assertBaseType isTuple typ

        assert (length es == length xs) ("does not satisfy " ++ show typ)
        vals <- forM (zip xs es) $ \((s, t), e) ->
            valAsType t =<< cmpExpr e 

        tup <- valLocal typ
        zipWithM_ (tupleSet tup) [0..] =<< zipWithM (valAsType . snd) xs vals
        return tup
