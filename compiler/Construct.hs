{-# LANGUAGE FlexibleContexts #-}
module Construct where

import Control.Monad

import qualified LLVM.AST.Type as LL
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Constant       
import qualified LLVM.AST.Constant as C
import LLVM.AST.Name

import Type
import State
import Monad
import Tuple
import Value
import Typeof
import Trace
import Table
import Funcs
import Error
import qualified AST as S


valZero :: InsCmp CompileState m => Type -> m Value
valZero typ = trace ("valZero " ++ show  typ) $ do
    case typ of
        Typedef sym -> do
            ObType t namem <- look sym KeyType
            fmap (Val typ . valOp) $ valZero' namem =<< baseTypeOf t
        _ -> valZero' Nothing typ

    where
        valZero' :: InsCmp CompileState m => Maybe Name -> Type -> m Value
        valZero' namem base =
            case base of
                _ | isInt base   -> valInt typ 0
                _ | isFloat base -> valFloat typ 0.0
                Bool            -> valBool typ False
                Char            -> valChar typ '\0'
                Typedef sym     -> fmap (Val typ . valOp) $ valZero =<< baseTypeOf typ
                Array n t       -> Val typ . array . replicate n . toCons . valOp <$> valZero t
                Tuple ts        -> Val typ . struct namem False . map (toCons . valOp) <$> mapM valZero ts
                Table ts        -> do
                    let zi64 = toCons (int64 0)
                    zptrs <- map (C.IntToPtr zi64 . LL.ptr) <$> mapM opTypeOf ts
                    return $ Val typ $ struct namem False (zi64:zi64:zptrs)
                ADT tss
                    | isEnumADT base -> Val typ . valOp <$> valZero I64
                _ -> error ("valZero: " ++  show typ)

valCopy :: InsCmp CompileState m => Value -> m Value
valCopy val = trace "valCopy" $ do
    base <- baseTypeOf (valType val)
    case base of
        t | isSimple t -> valLoad val

        Table ts -> do
            len <- tableLen val
            tab <- tableMake (valType val) len

            forM_ (zip ts [0..]) $ \(t, i) -> do
                baseT <- baseTypeOf t
                valRow <- tableRow i val
                tabRow <- tableRow i tab

                if isSimple baseT
                then valMemCpy tabRow valRow len
                else for (valOp len) $ \op -> do
                    ptr <- valPtrIdx tabRow (Val I64 op)
                    valStore ptr =<< valCopy =<< valPtrIdx valRow (Val I64 op)

            valLoad tab


        Tuple ts -> do
            loc <- valLocal (valType val)
            forM_ (zip ts [0..]) $ \(t, i) ->
                tupleSet loc i =<< valCopy =<< tupleIdx i val
            valLoad loc

        _ -> fail $ "Can't handle copy: " ++ show (valType val)


valString :: InsCmp CompileState m => Type -> Value -> m Value
valString typ val = do
    let bufSize = 32
    
    assertBaseType (== Table [Char]) typ
    base <- baseTypeOf (valType val)

    tab <- tableMake typ (valI64 bufSize) 
    row <- tableRow 0 tab
    ptr <- bitcast (valLoc row) (LL.ptr LL.i8)
    op <- valOp <$> valLoad val

    case base of
        I64 -> do
            n <- Val I32 <$> snprintf ptr (int64 bufSize) "%d" [op]
            tableSetLen tab =<< valConstruct I64 [n]
        F64 -> do
            n <- Val I32 <$> snprintf ptr (int64 bufSize) "%f" [op]
            tableSetLen tab =<< valConstruct I64 [n]
        _ -> fail $ "No string function for: " ++ show (valType val)
        


    valLoad tab




valConstruct :: InsCmp CompileState m => Type -> [Value] -> m Value
valConstruct typ []       = trace "valConstruct" $ valZero typ
valConstruct typ (a:b:xs) = trace "valConstruct" $ tupleConstruct typ (a:b:xs)
valConstruct typ [val']   = trace "valConstruct" $ do
    val <- valLoad val'
    base <- baseTypeOf typ

    case base of
        t | isIntegral t || isFloat t -> valConvertNumber typ val

        Table [Char] -> do
            res <- lookm (Sym "string") $ KeyFunc [valType val] typ
            case res of
                Just (ObjFunc op) -> Val typ <$> call op [(valOp val, [])]
                Nothing -> valString typ val


        _ -> do
            assert (typ  == valType val) "mismatched types"
            Val typ . valOp <$> valLoad val
