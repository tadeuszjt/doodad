{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Compile where

import Data.List
import Data.Maybe
import Data.Char
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
import LLVM.AST.Global hiding (prefix)
import LLVM.IRBuilder.Instruction as LL
import qualified LLVM.AST.IntegerPredicate as P
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Context

import qualified AST
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
import Typeof
import Trace
import Interop
import ADT
import Array
import Builtin
import Symbol
import Sparse
import States
import qualified JIT



fnHdrToOp :: ModCmp CompileState m => [Type] -> Symbol -> [Type] -> Type -> m LL.Operand
fnHdrToOp paramTypes symbol argTypes returnType = do
    paramOpTypes <- map LL.ptr <$> mapM opTypeOf paramTypes
    argOpTypes   <- mapM opTypeOf argTypes
    returnOpType <- opTypeOf returnType
    let name = fnSymbolToName symbol
    return $ fnOp name (paramOpTypes ++ argOpTypes) returnOpType False


compile :: BoM s m => IRGenState -> JIT.Session -> m ([LL.Definition], CompileState)
compile irGenState session = do
    ((_, defs), state) <- runBoMTExcept (initCompileState (irModuleName irGenState) session) (runModuleCmpT emptyModuleBuilder cmp)
    return (defs, state)
    where
        cmp :: (MonadFail m, Monad m, MonadIO m) => ModuleCmpT CompileState m ()
        cmp = do
            mainOp <- func (LL.mkName "main")  [] LL.VoidType $ \_ -> do
                mapM_ (\(s, x) -> define s $ ObjField $ snd x) $ Map.toList $ irCtorDefs irGenState
                mapM_ (\(s, x) -> define s $ ObjType x)        $ Map.toList $ irTypeDefs irGenState
                --cmpTypeNames irGenState
                cmpDeclareExterns irGenState
                cmpFuncHdrs irGenState
                cmpFuncBodies irGenState

            void $ func (LL.mkName $ (irModuleName irGenState) ++ "..callMain")  [] LL.VoidType $ \_ -> do
                void $ call mainOp []


cmpTypeNames :: InsCmp CompileState m => IRGenState -> m ()
cmpTypeNames irGenState = withErrorPrefix "cmpTypeNames" $ do
    opType <- opTypeOf (Table [Char])
    llType <- typedef "string" (Just opType)
    modify $ \s -> s { typeNameMap = Map.insert (Table [Char]) llType (typeNameMap s) }

    forM_ (Map.toList $ irTypeDefs irGenState) $ \(symbol, typ) ->
        defineTypeName symbol typ
    where 
        defineTypeName :: InsCmp CompileState m => Symbol -> Type -> m ()
        defineTypeName symbol typ = do 
            case typ of
                I64 -> return ()
                I32 -> return ()
                Enum -> return ()
                Table [Char] -> return ()
                Table ts -> mapM_ verifyTypeName ts >> addDef symbol typ
                Sparse ts -> mapM_ verifyTypeName ts >> addDef symbol typ

                _ -> return ()
                Tuple ts -> mapM_ verifyTypeName ts >> addDef symbol typ
                ADT fs -> do 
                    return ()
--                    forM fs $ \f -> case f of 
--                        FieldType t -> verifyTypeName t
--                        FieldCtor ts -> mapM_ verifyTypeName ts
--                    addDef symbol typ

                _ -> error (show typ)

        addDef :: InsCmp CompileState m => Symbol -> Type -> m ()
        addDef symbol typ = do
            resm <- Map.lookup (Typedef symbol) <$> gets typeNameMap
            when (isNothing resm) $ do
                let name = mkNameFromSymbol symbol
                opType <- opTypeOf typ
                llType <- typedef name (Just opType)
                modify $ \s -> s { typeNameMap = Map.insert (Typedef symbol) llType (typeNameMap s) }
                

        verifyTypeName :: InsCmp CompileState m => Type -> m ()
        verifyTypeName typ = case typ of
            Char -> return ()
            F64 -> return ()
            F32 -> return ()
            Bool -> return ()
            I64 -> return ()
            Table ts -> mapM_ verifyTypeName ts
            Sparse ts -> mapM_ verifyTypeName ts
            Tuple ts -> mapM_ verifyTypeName ts
            Typedef s -> case Map.lookup s (irTypeDefs irGenState) of
                Just t -> defineTypeName s t
            _ -> error (show typ)


cmpDeclareExterns :: InsCmp CompileState m => IRGenState -> m ()
cmpDeclareExterns irGenState = withErrorPrefix "cmpDeclareExterns: " $ do
    forM_ (Map.toList $ irExternDefs irGenState) $
        \(symbol, (paramTypes, sym, argTypes, returnType)) -> withErrorPrefix (sym ++ " ") $ do
            --liftIO $ putStrLn $ show sym
            (flip catchError) (\e -> return ()) $ do
                let name = fnSymbolToName symbol
                paramOpTypes <- map LL.ptr <$> mapM opTypeOf paramTypes
                argOpTypes   <- mapM opTypeOf argTypes
                returnOpType <- opTypeOf returnType
                extern name (paramOpTypes ++ argOpTypes) returnOpType
                define symbol ObjFn


cmpFuncHdrs :: InsCmp CompileState m => IRGenState -> m ()
cmpFuncHdrs irGenState = do
    forM_ (Map.toList $ irFuncDefs irGenState) $ \(symbol, funcBody) -> do
        forM_ (funcArgs funcBody) $ \(AST.Param pos name typ) -> withPos pos $ do
            isDataType <- isDataType typ
            assert (not isDataType) "Cannot use a data type for an argument"
        define symbol ObjFn


cmpFuncBodies :: (MonadFail m, Monad m, MonadIO m) => IRGenState -> InstrCmpT CompileState m ()
cmpFuncBodies irGenState = do
    case irMainDef irGenState of
        Nothing   -> return ()
        Just body -> do 
            let paramTypes  = map typeof (funcParams body)
            let paramSymbols = map AST.paramName (funcParams body)
            case paramTypes of
                [] -> return ()
                [Typedef symbol] | sym symbol == "Io" -> do
                    loc <- newVal (Typedef symbol)
                    define (head paramSymbols) $ ObjVal loc

            mapM_ cmpStmt (funcStmts $ body)

    forM_ (Map.toList $ irFuncDefs irGenState) $ \(symbol, body) -> do
        let argTypes     = map typeof (funcArgs body)
        let argSymbols   = map AST.paramName (funcArgs body)
        let argNames     = map (ParameterName . mkBSS . Symbol.sym) argSymbols
        let paramTypes   = map typeof (funcParams body)
        let paramSymbols = map AST.paramName (funcParams body)
        let paramNames   = map (ParameterName . mkBSS . Symbol.sym) paramSymbols
        let retty        = funcRetty body

        returnOpType <- opTypeOf retty
        argOpTypes   <- mapM opTypeOf argTypes
        paramOpTypes <- map LL.ptr <$> mapM opTypeOf paramTypes

        ObjFn <- look symbol
        op <- fnHdrToOp paramTypes symbol argTypes retty
        let LL.ConstantOperand (C.GlobalReference _ name) = op

        void $ InstrCmpT . IRBuilderT . lift $ func name (zip (paramOpTypes ++ argOpTypes)  (paramNames ++ argNames)) returnOpType $ \argOps -> do
            emitBlockStart =<< fresh

            forM_ (zip3 paramTypes paramSymbols argOps) $ \(typ, symbol, op) -> do
                define symbol (ObjVal $ Pointer typ op)

            forM_ (zip3 argTypes (drop (length paramSymbols) argOps) argSymbols) $ \(typ, op, symbol) -> do
                loc <- newVal typ
                storeBasicVal loc (Value typ op)
                define symbol (ObjVal loc)


            mapM_ cmpStmt (funcStmts body)
            hasTerm <- hasTerminator
            if hasTerm then return ()
            else if retty == Void then retVoid
            else do 
                trapMsg "reached end of function"
                unreachable


trapBranch :: InsCmp CompileState m => LL.Name -> LL.Operand -> m () -> m ()
trapBranch endName cnd continue = do
    b <- icmp P.EQ cnd (bit 0)
    when_ b $ trap
    continue


cmpStmt :: InsCmp CompileState m => AST.Stmt -> m ()
cmpStmt stmt = trace "cmpStmt" $ withPos stmt $ case stmt of
    AST.Print pos exprs   -> do
        label "print"
        cmpPrint stmt

    AST.Typedef pos symbol anno -> return ()
    AST.Block stmts   -> mapM_ cmpStmt stmts
    AST.ExprStmt expr -> withErrorPrefix "exprStmt: " $ void $ cmpExpr expr

    AST.Data pos symbol typ mexpr -> do
        loc <- newVal typ
        case mexpr of
            Nothing -> return ()
            Just expr -> storeBasic loc =<< cmpExpr expr
        define symbol (ObjVal loc)

    AST.Assign pos pat expr -> withErrorPrefix "assign: " $ do
        val <- cmpExpr expr
        name <- freshName "trap" 
        exit <- freshName "exit" 
        cmpPattern pat val (trapBranch name) $ return ()

        br exit
        emitBlockStart name 
        trap 
        unreachable
        emitBlockStart exit

    AST.Set pos expr1 expr2 -> do
        label "set"
        loc <- cmpExpr expr1
        storeCopy loc =<< cmpExpr expr2

    AST.Return pos Nothing -> withErrorPrefix "return: " $ do
        label "return"
        retVoid
        emitBlockStart =<< fresh

    AST.Return pos (Just expr) -> withErrorPrefix "return: " $ do
        label "return_expr"
        ret . op =<< pload =<< cmpExpr expr
        emitBlockStart =<< fresh

    AST.If pos (AST.AExpr _ (AST.Match _ expr pat)) blk melse -> do
        val <- cmpExpr expr
        yes <- newBool True
        cmpPattern pat val when_ $ do
            storeCopyVal yes (mkBool False) 
            cmpStmt blk

        when (isJust melse) $ do
            y <- pload yes
            when_ (op y) $ cmpStmt (fromJust melse)

    AST.If pos expr blk melse -> do
        label "if"
        val <- pload =<< cmpExpr expr
        Bool <- baseTypeOf val
        if_ (op val) (cmpStmt blk) $ maybe (return ()) cmpStmt melse


    AST.While pos (AST.AExpr _ (AST.Match _ expr pat)) blk -> do
        label "while"
        cond <- freshName "while_cond"
        exit <- freshName "while_exit"

        br cond
        emitBlockStart cond

        b <- newVal Bool
        val <- cmpExpr expr
        cmpPattern pat val when_ $ do
            cmpStmt blk
            storeCopyVal b (mkBool True)
        b' <- pload b
        condBr (op b') cond exit
        
        emitBlockStart exit



    AST.While pos cnd blk -> do
        label "while"
        cond <- freshName "while_cond"
        body <- freshName "while_body"
        exit <- freshName "while_exit"

        br cond
        emitBlockStart cond
        val <- pload =<< cmpExpr cnd
        Bool <- baseTypeOf val
        condBr (op val) body exit
        
        emitBlockStart body
        cmpStmt blk
        br cond
        emitBlockStart exit

    AST.Switch _ expr cases -> do
        label "switch"
        val <- cmpExpr expr
        let cases' = [(fmap op (switchHelper pat val stmt), return ()) | (pat, stmt) <- cases]
        let trap   = trapMsg ("switch match failure at: " ++ show (textPos stmt))
        switch_ $ cases' ++ [(return (bit 1), trap)]

    AST.For _ expr mpat blk -> do
        label "for"
        val <- cmpExpr expr
        base <- baseTypeOf val

        idxType <- case base of 
            Range t -> return t
            _       -> return I64

        idx <- newVal idxType
        case base of
            Range _ -> storeCopy idx =<< rangeStart val
            _       -> return ()

        cond <- freshName "for_cond"
        body <- freshName "for_body"
        exit <- freshName "for_exit"

        br cond

        -- check if index is still in range
        emitBlockStart cond
        end <- case base of
            Range t -> rangeEnd val
            Table ts -> tableLen val
            Array n t -> newI64 n
            _ -> error (show base)
        ilt <- pload =<< newInfix AST.LT idx end
        condBr (op ilt) body exit


        -- for loop body
        emitBlockStart body
        patMatched <- newBool False

        case mpat of
            Nothing -> do 
                cmpStmt blk
                storeCopyVal patMatched (mkBool True)
            Just pat -> do 
                patVal <- case base of
                    Table ts -> do
                        [elm] <- tableColumn val =<< pload =<< toType I64 idx
                        return elm
                    Range t   ->  return idx
                    Array n t -> arrayGetElem val =<< pload =<< toType I64 idx
                    _ -> error (show base)

                cmpPattern pat patVal when_ $ do 
                    cmpStmt blk
                    storeCopyVal patMatched (mkBool True)

        increment idx
        b <- pload patMatched
        condBr (op b) cond exit

        emitBlockStart exit

    _ -> error (show stmt)
    where
        switchHelper p v stmt = do
            matched <- newBool False
            cmpPattern p v when_ $ do 
                cmpStmt stmt
                storeCopyVal matched (mkBool True)
            pload matched


        label :: InsCmp CompileState m => String -> m ()
        label str = do
            return ()
--            let pos = textPos stmt
--            name <- freshName $ mkBSS $ str ++ " " ++ show (textLine pos)
--            --printf (str ++ " " ++ show (Error.textLine pos) ++ "\n") []
--            br name
--            emitBlockStart name

readString :: String -> String
readString str = case str of
    ('\\' : '\\' : ss) -> '\\' : (readString ss)
    ('\\' : 'n' : ss) -> '\n' : (readString ss)
    ('\\' : 't' : ss) -> '\t' : (readString ss)
    ('\\' : '0' : ss) -> '\0' : (readString ss)
    ('\\' : '"' : ss) -> '"' : (readString ss)
    (s:ss)            -> s : (readString ss)
    []                ->  []

cmpExpr :: InsCmp CompileState m =>  AST.Expr -> m Pointer
cmpExpr (AST.AExpr exprType expr) = withErrorPrefix "expr: " $ withPos expr $ withCheck exprType $ case expr of
    AST.Bool pos b               -> (toType exprType =<< newBool b)
    AST.Char pos c               -> (toType exprType =<< newChar c)
    AST.Tuple pos [expr]         -> cmpExpr expr
    AST.Float p f                -> newFloat exprType f
    AST.Field pos expr symbol    -> (tupleField symbol =<< cmpExpr expr)
    AST.Null p                   -> adtNull exprType
    AST.Conv pos typ exprs      -> construct typ =<< mapM cmpExpr exprs
    AST.Prefix pos operator expr -> do 
        val <- newVal exprType
        storeBasicVal val =<< prefix operator =<< cmpExpr expr
        return val

    AST.Array pos exprs -> do
        base <- baseTypeOf exprType
        case base of
            Array n t -> do
                assert (n == length exprs) "invalid array length"
                vals <- mapM cmpExpr exprs
                array <- newValNonZero exprType
                forM_ (zip vals [0..]) $ \(val, i) -> do
                    loc <- arrayGetElem array (mkI64 i)
                    storeCopy loc val
                return array

            _ -> error (show base)



    AST.Range pos Nothing _ Nothing -> fail "Range expression must contain maximum"
    AST.Range pos Nothing mexpr1 (Just expr2) -> do
        end <- pload =<< cmpExpr expr2
        start <- pload =<< maybe (newVal $ typeof end) cmpExpr mexpr1
        newRange start end
    AST.Range pos (Just expr) margStart margEnd -> do
        val <- cmpExpr expr
        base <- baseTypeOf val

        -- default 0 because all types index 0
        startVal <- case base of
            Range t -> rangeStart val
            _       -> newI64 0
        start <- case margStart of
            Nothing  -> pload startVal
            Just arg -> do 
                arg' <- cmpExpr arg
                pload =<< Builtin.max arg' =<< toType (typeof arg') startVal

        endVal <- case base of
            Range t   -> rangeEnd val
            Array n t -> newI64 n
            Table ts  -> tableLen val
            _ -> error (show base)
        end <- case margEnd of
            Nothing  -> pload endVal
            Just arg -> do 
                arg' <- cmpExpr arg
                pload =<< Builtin.min arg' =<< toType (typeof arg') endVal
            
        newRange start end

    AST.Builtin pos [expr] "push" [] -> do
        loc <- cmpExpr expr
        base <- baseTypeOf loc
        val <- newVal exprType
        case base of
            Table ts  -> storeCopyVal val =<< convertNumber (typeof val) =<< tablePush loc
            Sparse ts -> storeCopyVal val =<< convertNumber (typeof val) =<< sparsePush loc =<< mapM newVal ts
        return val

    AST.Builtin pos [expr] "push" exprs -> do
        loc <- cmpExpr expr
        base <- baseTypeOf loc
        val <- newVal exprType
        case base of
            Table ts -> do
                vals <- mapM cmpExpr exprs
                assert (map typeof vals == ts) "mismatched argument types"
                len <- pload =<< tableLen loc
                tableResize loc =<< intInfix AST.Plus len (mkI64 1)
                ptrs <- tableColumn loc len
                zipWithM_ storeCopy ptrs vals
                storeCopyVal val =<< convertNumber exprType len

            Sparse ts -> do 
                n <- sparsePush loc =<< mapM cmpExpr exprs
                storeBasicVal val =<< convertNumber exprType n
        return val


    AST.Builtin pos [expr] "pop" [] -> do
        table <- cmpExpr expr
        vals <- tablePop table
        case vals of 
            []  -> error ""
            [v] -> return v
            vs  -> do 
                tuple <- newVal (Tuple $ map typeof vs)
                forM (zip vs [0..]) $ \(v, i) -> do 
                    ptr <- tupleIdx i tuple
                    storeCopy ptr v
                return tuple

    AST.Builtin pos [expr] "clear" [] -> do
        assert (exprType == Void) "clear is a void expression"
        tab <- cmpExpr expr
        tableResize tab (mkI64 0)
        return $ Pointer Void undefined

    AST.Builtin pos [expr1] "delete" [expr2] -> do
        assert (exprType == Void) "delete returns void"
        val <- cmpExpr expr1
        arg <- cmpExpr expr2
        base <- baseTypeOf val
        case base of
            Sparse ts -> sparseDelete val =<< pload arg
            Table ts  -> tableDelete val =<< pload arg
        return $ Pointer Void undefined
                
    AST.Int p n -> do
        base <- baseTypeOf exprType
        val <- newVal exprType
        case base of
            _ | isInt base   -> storeBasicVal val =<< convertNumber exprType (mkI64 n)
            _ | isFloat base -> storeBasic val =<< newFloat exprType (fromIntegral n)
            _ | base == Char -> storeBasic val =<< toType exprType =<< newChar (chr $ fromIntegral n)
            _ | otherwise    -> fail $ "invalid base type of: " ++ show base
        return val

    AST.Infix pos AST.AndAnd exprA exprB -> do
        Bool <- baseTypeOf exprType

        result <- newBool False
        valA <- pload =<< cmpExpr exprA
        Bool <- baseTypeOf valA

        when_ (op valA) $ do
            storeCopy result =<< cmpExpr exprB

        return result
        
    AST.Infix pos op exprA exprB -> do
        valA <- cmpExpr exprA
        valB <- cmpExpr exprB
        newInfix op valA valB

    AST.Ident pos symbol -> do
        obj <- look symbol
        case obj of
            ObjVal loc -> return loc
            
    AST.String pos s -> do
        let s' = readString s -- replaces \n with actual newline char
        base <- baseTypeOf exprType
        case base of
            Table [Char] -> do
                tab <- newVal exprType
                cap <- tableCap tab
                len <- tableLen tab
                storeBasicVal cap (mkI64 $ length s')
                storeBasicVal len (mkI64 $ length s')
                tableSetRow tab 0 . Pointer Char =<< getStringPointer s'

                tab2 <- newVal exprType
                storeCopy tab2 tab
                return tab2

            Array n Char -> do
                arr <- newVal exprType
                src <- Pointer Char <$> getStringPointer s'
                dst <- arrayGetElem arr (mkI64 0)
                memCpy dst src (mkI64 $ length s')
                return arr

            _ -> fail ("AST.String: " ++ show base)
                

    AST.Call pos params symbol args  -> withErrorPrefix "call: " $ do
        ps <- mapM cmpExpr params
        as <- mapM cmpExpr args

        asOps <- map op <$> mapM pload as
        obj <- look symbol
        val <- case obj of
            ObjFn -> do
                op <- fnHdrToOp (map typeof ps) symbol (map typeof as) exprType
                v <- Value exprType <$> call op [(o, []) | o <- (map loc ps) ++ asOps]
                case exprType of 
                    Void -> return $ Pointer Void undefined
                    _ -> do
                        loc <- newVal exprType
                        storeBasicVal loc v
                        return loc
            ObjField i -> do
                base <- baseTypeOf exprType
                case base of
                    Enum  -> newEnum exprType i
                    ADT fs -> do 
                        adt <- newVal exprType 
                        adtSetEnum adt i
                        assert (ps == []) "Invalid constructor"
                        case fs !! i of 
                            FieldNull    -> return ()
                            FieldCtor [t] -> do
                                assert ([t] == map typeof as) "Invalid constructor"
                                ptr <- adtField adt i
                                storeCopy ptr (head as)
                            FieldCtor ts -> do
                                assert (ts == map typeof as) "Invalid constructor"
                                tupPtr <- adtField adt i
                                forM_ (zip as [0..]) $ \(a, i) -> do 
                                    tupMember <- tupleIdx i tupPtr
                                    storeCopy tupMember a
                        return adt
                    _ -> error (show base)
            _ -> error (show obj)

        case exprType of 
            Void -> return $ Pointer Void undefined
            _    -> return val


    AST.Builtin pos [] "conv" [expr] -> construct exprType =<< mapM cmpExpr [expr]
    
    AST.Builtin pos [expr] "len" [] -> do
        val <- cmpExpr expr
        base <- baseTypeOf val
        v <- case base of
            Table _   -> convertNumber exprType =<< pload =<< tableLen val
            Array n t -> convertNumber exprType (mkI64 n)
            Sparse ts -> convertNumber exprType =<< sparseLen val
            _       -> fail ("cannot take length of type " ++ show (typeof val))
        result <- newVal exprType 
        storeBasicVal result v 
        return result

    AST.Tuple pos exprs -> do
        Tuple ts <- baseTypeOf exprType
        vals <- mapM cmpExpr exprs
        assert (ts == map typeof vals) $
            "Incorrect val types: " ++ show ts ++ ", " ++ show (map typeof vals)

        tup <- newVal exprType
        forM_ (zip vals [0..]) $ \(v, i) -> do
            ptr <- tupleIdx i tup
            storeBasic ptr v
        return tup

    AST.Subscript pos expr idxExpr -> withErrorPrefix "subscript: " $ do
        val <- cmpExpr expr
        idx <- cmpExpr idxExpr
        base <- baseTypeOf val
        case base of
            Table _   -> (\[x] -> x) <$> (tableColumn val =<< pload idx)
            Array _ _ -> arrayGetElem val =<< pload idx
            Map _ _ -> mapIndex val idx
            Sparse _  -> do
                table <- sparseTable val
                [ptr] <- tableColumn table =<< pload idx
                return $ ptr
                
            Range t -> do
                Bool <- baseTypeOf exprType
                idxGtEqStart <- newInfix AST.GTEq idx =<< toType (typeof idx) =<< rangeStart val
                idxLtEnd     <- newInfix AST.LT idx =<< toType (typeof idx) =<< rangeEnd val
                newInfix AST.AndAnd idxLtEnd idxGtEqStart
                
    AST.Initialiser pos exprs -> do
        base <- baseTypeOf exprType
        vals <- mapM cmpExpr exprs
        case base of
            Table [t] -> do
                table <- newTable exprType (mkI64 $ length vals)
                forM_ (zip vals [0..]) $ \(val, i) -> do
                    [ptr] <- tableColumn table (mkI64 i)
                    storeCopy ptr val
                return table


            Array n t -> do
                assert (length vals == n) "Invalid array length"
                arr <- newVal exprType
                forM_ (zip vals [0..]) $ \(val, i) -> do
                    ptr <- arrayGetElem arr (mkI64 i)
                    storeBasic ptr val
                return (arr)

            _ -> fail $ "invalid initialiser base type: " ++ show base

    AST.Builtin pos [expr] "unsafe_ptr" [] -> do
        val <- cmpExpr expr
        let UnsafePtr = exprType
        base <- baseTypeOf val
        case base of
            Table [Char] -> do
                [elm] <- tableColumn val (mkI64 0)
                val <- Value UnsafePtr <$> bitcast (loc elm) (LL.ptr LL.i8)
                result <- newVal exprType
                storeBasicVal result val 
                return result

            _ -> error $ show base

    AST.Builtin pos [] "unsafe_ptr_from_int" [expr] -> do
        val <- convertNumber I64 =<< pload =<< cmpExpr expr
        UnsafePtr <- baseTypeOf exprType
        val <- Value exprType <$> inttoptr (op val) (LL.ptr LL.i8)
        result <- newVal exprType
        storeBasicVal result val 
        return result

    _ -> fail ("invalid expression: " ++ show expr)
    where
        withCheck :: InsCmp CompileState m => Type -> m Pointer -> m Pointer
        withCheck typ m = do
            val <- m
            assert (typeof val == typ) $ "Expression compiled to: " ++ show (typeof val) ++ " instead of: " ++ show typ
            return val





cmpPatTuple :: InsCmp CompileState m => [AST.Pattern] -> Pointer -> (LL.Operand -> m () -> m ()) -> m () -> m ()
cmpPatTuple pats tup branch mMatched = do 
    len <- tupleLength =<< pload tup
    assert (len == length pats) "incorrect tuple length"
    matchField len 0
    where 
        matchField len i
            | i == len  = mMatched
            | otherwise = do
                ptr <- tupleIdx i tup
                cmpPattern (pats !! i) ptr branch $ matchField len (i + 1)



cmpPatArray :: InsCmp CompileState m => [[AST.Pattern]] -> Pointer -> (LL.Operand -> m () -> m ()) -> m () -> m ()
cmpPatArray patss val branch mMatched = do
    Table ts <- baseTypeOf val
    assert (length patss == length ts) "Invalid number of rows"
    forM_ (zip ts [0..]) $ \(t, i) -> do
        assert (length (patss !! i) == length (head patss)) "row length mismatch"

    lenMatch <- intInfix AST.EqEq (mkI64 $ length $ head patss) =<< pload =<< tableLen val
    when_ (op lenMatch) $ matchRows 0 mMatched
    where 
        matchRows r mMatched 
            | r == length patss = mMatched
            | otherwise = do 
                row <- tableRow r val
                matchTableRow (patss !! r) row 0 $ matchRows (r + 1) mMatched

        matchTableRow pats row c mMatched
            | c == length pats = mMatched
            | otherwise = do
                ptr <- advancePointer row (mkI64 c)
                cmpPattern (pats !! c) ptr branch $ matchTableRow pats row (c + 1) mMatched


cmpPatAdtField :: InsCmp CompileState m => Symbol -> [AST.Pattern] -> Pointer -> (LL.Operand -> m () -> m()) -> m () -> m ()
cmpPatAdtField symbol pats val branch mMatched = do
    ADT fs <- baseTypeOf val
    obj <- look symbol
    i <- case obj of 
        ObjType _  -> adtTypeField (typeof val) (Typedef symbol)
        ObjField i -> return i

    enumMatch <- intInfix AST.EqEq (mkI64 i) =<< adtEnum =<< pload val
    branch (op enumMatch) $ do
        field <- adtField val i -- tuple or type
        case pats of 
            [pat] -> cmpPattern pat field branch mMatched
            pats -> do 
                ts <- case obj of
                    ObjType _ -> baseTypeOf field >>= \(Tuple ts) -> return ts
                    ObjField _ -> return $ let FieldCtor ts = fs !! i in ts
                assert (length pats == length ts) "invalid ADT pattern"
                matchArg 0 field
    where 
        matchArg j field
            | j == length pats = mMatched
            | otherwise = do 
                arg <- tupleIdx j field
                cmpPattern (pats !! j) arg branch $ matchArg (j + 1) field


cmpPatField :: InsCmp CompileState m => Symbol -> [AST.Pattern] -> Pointer -> (LL.Operand -> m () -> m()) -> m () -> m ()
cmpPatField symbol pats val branch mMatched = do
    base <- baseTypeOf val 
    case base of
        ADT fs -> cmpPatAdtField symbol pats val branch mMatched
        Enum -> do
            assert (pats == []) "enum pattern with args"
            ObjField i <- look symbol
            enumMatched <- intInfix AST.EqEq (mkI64 i) =<< adtEnum =<< pload val
            branch (op enumMatched) mMatched


cmpPattern :: InsCmp CompileState m => AST.Pattern -> Pointer -> (LL.Operand -> m () -> m ()) -> m () -> m ()
cmpPattern pattern val branch mMatched = withErrorPrefix "pattern: " $ withPos pattern $ case pattern of
    AST.PatIgnore _     -> mMatched
    AST.PatTuple _ pats -> cmpPatTuple pats val branch $ mMatched

    AST.PatLiteral expr -> do 
        eq <- pload =<< newInfix AST.EqEq val =<< cmpExpr expr
        branch (op eq) mMatched


    AST.PatNull _ -> do -- null
        i <- adtNullField (typeof val)
        enumMatch <- intInfix AST.EqEq (mkI64 i) =<< adtEnum =<< pload val
        branch (op enumMatch) mMatched

    AST.PatAnnotated pat typ -> do -- a:i32
        assert (typeof val == typ) "pattern type mismatch"
        cmpPattern pat val branch mMatched

    AST.PatGuarded _ pat expr mpat -> do -- a | a > 0
        cmpPattern pat val branch $ do
            case mpat of 
                Nothing -> do 
                    b <- pload =<< cmpExpr expr
                    branch (op b) mMatched

                Just pat -> do 
                    v <- cmpExpr expr
                    cmpPattern pat v branch mMatched

    AST.PatIdent _ symbol -> trace ("cmpPattern " ++ show pattern) $ do -- a
        base <- baseTypeOf val
        False <- isDataType (typeof val)
        loc <- newValNonZero (typeof val)
        storeCopy loc val
        define symbol (ObjVal loc)
        mMatched

    AST.PatArray _ patss -> do 
        base <- baseTypeOf val
        case base of
            Sparse ts -> do 
                tab <- sparseTable val
                cmpPattern pattern tab branch mMatched
            Table ts -> cmpPatArray patss val branch mMatched
            Array n t -> do 
                tab <- newVal (Table [t])
                len <- tableLen tab
                cap <- tableCap tab
                storeCopyVal len (mkI64 n)
                storeCopyVal cap (mkI64 n)
                tableSetRow tab 0 =<< arrayGetElem val (mkI64 0)
                cmpPatArray patss tab branch mMatched


            _ -> fail (show base)

    AST.PatField _ symbol pats -> cmpPatField symbol pats val branch mMatched


    AST.PatTypeField _ typ pat -> do -- char(c)
        i <- adtTypeField (typeof val) typ
        enumMatch <- intInfix AST.EqEq (mkI64 i) =<< adtEnum =<< pload val
        branch (op enumMatch) $ do
            field <- adtField val i
            cmpPattern pat field branch mMatched

                    
cmpPrint :: InsCmp CompileState m => AST.Stmt -> m ()
cmpPrint (AST.Print pos exprs) = trace "cmpPrint" $ do
    prints =<< mapM cmpExpr exprs
    where
        prints :: InsCmp CompileState m => [Pointer] -> m ()
        prints []     = void $ printf "\n" []
        prints [val]  = valPrint "\n" val
        prints (v:vs) = valPrint ", " v >> prints vs
