module CmpADT where

import Data.Word
import Data.Ord
import Data.List
import Control.Monad
import Control.Monad.State

import LLVM.AST.Type
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant

import qualified AST as S
import Type
import CmpMonad
import CmpValue
import CmpFuncs


cmpDataDef :: S.Stmt -> Instr ()
cmpDataDef (S.Datadef pos symbol datas) = withPos pos $ do
    checkUndefined symbol
    name <- freshName (mkBSS symbol)
    let dataTyp = Typedef symbol

    enumTyp <- case length datas of
        x
            | x < 2^8  -> return I64 -- I8 causes seq fault
            | x < 2^16 -> return I64
            | x < 2^32 -> return I64
            | x < 2^64 -> return I64
    
    memTyps <- forM datas $ \dat -> case dat of
        S.DataIdent p sym       -> return (Tuple (Just name) [enumTyp])
        S.DataFunc p sym params -> return $ Tuple (Just name) (enumTyp : map S.paramType params)

    memSizes <- mapM sizeOf =<< mapM opTypeOf memTyps
    let (_, memMaxTyp) = maximumBy (comparing fst) (zip memSizes memTyps)
    let dataConcTyp = memMaxTyp

    forM_ (zip3 memTyps datas [0..]) $ \(typ, dat, i) -> do
        checkUndefined (S.dataSymbol dat)
        case dat of
            S.DataIdent p sym -> withPos p $ do
                checkUndefined sym
                addSymObj sym KeyDataCons $ ObjDataCons dataTyp typ (fromIntegral i)
                addSymObj sym (KeyFunc []) $ ObjInline $ \[] -> do
                    tup@(Ptr _ _) <- valLocal dataConcTyp
                    valTupleSet tup 0 (valInt enumTyp i)
                    return (tup { valType = dataTyp })

            S.DataFunc p sym params -> withPos p $ do
                checkUndefined sym
                let paramSymbols = map S.paramName params
                let paramTypes   = map S.paramType params
                pushSymTab
                forM_ (zip paramSymbols paramTypes) $ \(s, t) ->
                    checkUndefined s
                popSymTab
                addSymObj sym KeyDataCons $ ObjDataCons dataTyp typ (fromIntegral i)
                addSymObj sym (KeyFunc paramTypes) $ ObjInline $ \args -> do
                    tup@(Ptr _ _) <- valLocal dataConcTyp
                    valTupleSet tup 0 (valInt enumTyp i)
                    ptr <- valCast typ tup
                    forM_ (zip args [1..]) $ \(arg, i) -> valTupleSet ptr i arg
                    return (tup { valType = dataTyp })


    addSymObj symbol KeyType $ ObjData 
        { dataConcTyp = dataConcTyp
        , dataPrintFn = printFn memTyps
        }
    addSymObjReq symbol KeyType name
    dataOpType <- opTypeOf dataConcTyp
    addAction name (typedef name $ Just dataOpType)

    where
        printFn :: [ValType] -> Value -> Instr ()
        printFn memTyps dat@(Ptr _ _) = do
            let memSymbols = map S.dataSymbol datas

            casesM <- forM (zip3 memSymbols memTyps [0..]) $ \(sym, typ, i) -> do
                let cmpCnd = do
                    tup <- valCast typ dat
                    en <- valTupleIdx 0 tup
                    Val Bool cnd <- valsEqual en $ valInt (valType en) i
                    return cnd

                let cmpStmt = do
                    tup <- valCast typ dat
                    len <- valLen tup
                    printf (sym ++ if len > 1 then "(" else "") []
                    forM_ [1..len-1] $ \i -> do
                        let app = if i < len-1 then ", " else ")"
                        valPrint app =<< valTupleIdx (fromIntegral i) tup

                return (cmpCnd, cmpStmt)

            switch_ casesM
