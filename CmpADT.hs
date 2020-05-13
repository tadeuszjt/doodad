module CmpADT where

import Data.Word
import Control.Monad

import LLVM.AST.Type
import LLVM.IRBuilder.Instruction

import qualified AST as S
import CmpMonad
import CmpValue
import CmpFuncs

cmpDataDef :: S.Stmt -> Instr ()
cmpDataDef (S.Datadef pos symbol datas) = withPos pos $ do
    checkUndefined symbol
    let dataTyp = Typedef symbol

    enumTyp <- case length datas of
        x
            | x < 2^8  -> return I8
            | x < 2^16 -> return I32
            | x < 2^32 -> return I32
            | x < 2^64 -> return I64
    
    enumOpTyp <- opTypeOf enumTyp
    enumSize <- sizeOf enumOpTyp


    memTyps <- forM datas $ \dat -> case dat of
        S.DataIdent p sym       -> return (Tuple [enumTyp])
        S.DataFunc p sym params -> return $ Tuple (enumTyp : map (fromASTType . S.paramType) params)

        
    memSizes <- mapM sizeOf =<< mapM opTypeOf memTyps
    let memMaxSize = maximum memSizes
    let dataConcTyp = Array memMaxSize I8

    addSymObj symbol KeyType $ ObjData 
        { dataConcTyp = dataConcTyp
        }
    
    forM_ (zip3 memTyps datas [0..]) $ \(typ, dat, i) -> do
        checkUndefined (S.dataSymbol dat)
        case dat of
            S.DataIdent p sym -> withPos p $ do
                addSymObj sym (KeyFunc []) $ ObjInline $ \[] -> do
                    Ptr _ loc <- valLocal dataConcTyp
                    ptr <- bitcast loc (ptr enumOpTyp)
                    valStore (Ptr enumTyp ptr) (consInt enumTyp i)
                    return (Ptr dataTyp loc)

            S.DataFunc p sym params -> withPos p $ do
                let paramSymbols = map S.paramName params
                let paramTypes   = map (fromASTType . S.paramType) params
                pushSymTab
                forM_ paramSymbols $ \sym -> checkUndefined sym
                popSymTab
                addSymObj sym (KeyFunc paramTypes) $ ObjInline $ \args -> do
                    Ptr _ loc <- valLocal dataConcTyp
                    ptr1 <- bitcast loc (ptr enumOpTyp) 
                    valStore (Ptr enumTyp ptr1) (consInt enumTyp i)

                    opTyp <- opTypeOf typ
                    ptr2 <- bitcast loc (ptr opTyp)
                    let val = Ptr typ ptr2
                    forM_ (zip args [0..]) $ \(arg, ai) ->
                        valTupleSet val (ai + 1) arg

                    return (Ptr dataTyp loc)


--cmpTopStmt (S.Datadef pos symbol datas) = withPos pos $ do
--    checkUndefined symbol
--
--    (nameStrs, nameStrLens) <- fmap unzip $ forM (map S.dataSymbol datas) $ \sym -> do
--        checkUndefined sym
--        return (sym ++ "\0", fromIntegral $ length sym + 1)
--    let numIdxs = fromIntegral (length nameStrLens)
--    let (_, idxs) = foldl (\(n, a) l -> (n+l, a ++ [n])) (0, []) nameStrLens
--    strName <- freshName $ mkBSS (symbol ++ "str")
--    idxName <- freshName $ mkBSS (symbol ++ "idx")
--    let (strDef, strOp) = stringDef strName (concat nameStrs)
--    let (idxDef, idxOp) = globalDef idxName (ArrayType numIdxs i64) $ Just $ C.Array i64 $ map (C.Int 64) idxs
--    addAction strName (emitDefn strDef)
--    addAction idxName (emitDefn idxDef)
--
--
--    let n = 4
--
--    typs <- forM (zip datas [0..]) $ \(d, i) -> cmpData d i
--    sizes <- mapM sizeOf =<< mapM opTypeOf typs
--
--    addSymObj symbol KeyType $ ObjData (maximum sizes) (Val String strOp) (Ptr (Array numIdxs I64) idxOp) 
--    addSymObjReq symbol KeyType strName
--    addSymObjReq symbol KeyType idxName
--
--    where
--        cmpData :: S.Data -> Integer -> Instr ValType
--        cmpData (S.DataIdent p sym) i = withPos p $ do
--            checkUndefined sym
--            let val = Val (Typedef symbol) (array [toCons (int64 i)])
--            addSymObj sym (KeyFunc []) (ObjVal val)
--            addSymObj sym KeyVal       (ObjVal val)
--            return I64
--
--        cmpData (S.DataFunc p sym params) i = withPos p $ do
--            checkUndefined sym
--            let paramSymbols  = map S.paramName params
--            let paramASTTypes = map S.paramType params
--            let paramTypes    = map fromASTType paramASTTypes
--            pushSymTab 
--            forM_ (paramSymbols) $ \(sym) -> checkUndefined sym
--            popSymTab
--
--            let typ = Tuple (I64 : paramTypes)
--            opTyp <- opTypeOf typ
--            val <- Val (Typedef symbol) <$> fmap cons (zeroOf typ)
--            addSymObj sym (KeyFunc paramTypes) $ ObjInline $ \args -> do
--                loc <- valLocal typ
--                valTupleSet loc 0 (Val I64 $ int64 i)
--                forM_ (zip args [0..]) $ \(a, i) ->
--                    valTupleSet loc (i + 1) a
--                v <- valLoad loc
--                return $ v { valType = Typedef symbol }
--                
--            return $ Tuple (I64 : paramTypes)
