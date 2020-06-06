module CmpADT where

import Data.Word
import Data.Ord
import Data.List
import Control.Monad
import Control.Monad.State

import LLVM.AST.Type hiding (Type)
import LLVM.AST.Name
import LLVM.AST hiding (Type)
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant

import qualified AST as S
import Type
import Print
import CmpMonad
import Value
import CmpFuncs


cmpDataDef :: S.Stmt -> Instr ()
cmpDataDef (S.Datadef pos symbol datas) = withPos pos $ do
    enumTyp <- case length datas of
        x
            | x < 2^8  -> return I8 -- I8 causes seq fault
            | x < 2^16 -> return I16
            | x < 2^32 -> return I32
            | x < 2^64 -> return I64
    
    memTyps <- forM datas $ \dat -> case dat of
        S.DataIdent p sym       -> return (Tuple Nothing [enumTyp])
        S.DataFunc p sym params -> return $ Tuple Nothing (enumTyp : map S.paramType params)

    memSizes <- mapM sizeOf memTyps
    let memMaxType@(Tuple Nothing ts) = snd $ maximumBy (comparing fst) (zip memSizes memTyps)

    opTyp <- opTypeOf memMaxType
    name <- freshName (mkBSS symbol)
    addAction name $ typedef name (Just opTyp)

    let dataTyp = Tuple (Just name) ts
    let dataDef = Typedef symbol

    forM_ (zip3 memTyps datas [0..]) $ \(typ, dat, i) -> do
        case dat of
            S.DataIdent p sym -> withPos p $ do
                addSymObj sym KeyDataCons $ ObjDataCons dataDef typ (fromIntegral i)
                let inline = ObjInline $ \[] -> do
                    tup@(Ptr _ _) <- valLocal dataTyp
                    valTupleSet tup 0 (valInt enumTyp i)
                    return (tup { valType = dataDef })
                addSymObj sym KeyVal inline
                addSymObj sym (KeyFunc []) inline

            S.DataFunc p sym params -> withPos p $ do
                let paramSymbols = map S.paramName params
                let paramTypes   = map S.paramType params

                addSymObj sym KeyDataCons $ ObjDataCons dataDef typ (fromIntegral i)
                addSymObj sym (KeyFunc paramTypes) $ ObjInline $ \args -> do
                    tup@(Ptr _ _) <- valLocal dataTyp
                    valTupleSet tup 0 (valInt enumTyp i)
                    ptr <- valCast typ tup
                    forM_ (zip args [1..]) $ \(arg, i) -> valTupleSet ptr i arg
                    return (tup { valType = dataDef })

    addSymObj symbol KeyType $ ObjData { dataConcTyp = dataTyp, dataPrintFn = printFn memTyps }
    addSymObjReq symbol KeyType name

    where
        printFn :: [Type] -> Value -> Instr ()
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
