module Builtin where

import Control.Monad

import qualified CAst as C
import Type
import Symbol
import CGenerate
import CBuilder


builtinLen :: Value -> Generate Value
builtinLen val = case val of
    Ref typ expr -> do
        base <- baseTypeOf typ
        return $ case base of
            TypeApply (Sym ["Table"]) _ -> Value I64 (C.PMember expr "len")

    Value typ expr -> do
        base <- baseTypeOf typ
        return $ case base of
            TypeApply (Sym ["Table"]) _ -> Value I64 (C.Member expr "len")
            Slice t                   -> Value I64 (C.Member expr "len")
    --        Type.Array n t -> return $ Value I64 $ C.Int (fromIntegral n)
            _ -> error (show base)



adtEnum :: Value -> Generate Value
adtEnum val = do
    TypeApply (Sym ["Sum"]) _ <- baseTypeOf val
    case val of
        Value _ expr -> return $ Value I64 $ C.Member expr "en"
        Ref _ expr   -> return $ Value I64 $ C.PMember expr "en"


builtinTableAppend :: Value -> Generate ()
builtinTableAppend (Ref typ expr) = do
    TypeApply (Sym ["Table"]) t <- baseTypeOf typ

    let len    = C.Member (C.Deref expr) "len"
    let newLen = (C.Infix C.Plus len $ C.Int 1)
    let cap    = C.Member (C.Deref expr) "cap"
    
    -- realloc if needed
    if_ (Value Type.Bool $ C.Infix C.GTEq len cap) $ do
        appendElem $ C.Set cap (C.Infix C.Times newLen (C.Int 2))

        let pMem = C.PMember expr "r0"
        let elemSize = C.Sizeof $ C.Deref $ C.PMember expr "r0"
        let newSize = C.Infix C.Times cap elemSize
        let dataSize = C.Infix C.Times len elemSize
        appendElem $ C.Set pMem $ C.Call "GC_realloc" [pMem, newSize]

    void $ appendElem $ C.ExprStmt $ C.Increment $ C.PMember expr "len"





builtinArrayAt :: Value -> Value -> Generate Value
builtinArrayAt value idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    TypeApply (Sym ["Array"]) [t, Size n] <- baseTypeOf value
    base <- baseTypeOf t

    case value of
        Value _ expr -> case base of
            TypeApply _ _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.Member expr "arr")
                (valExpr idx)
            TypeApply (Sym ["Tuple"]) _ -> error "TODO"
            x -> error (show x)
        Ref _ expr -> case base of
            x | isSimple x -> return $ Ref t $ C.Address $ C.Subscript
                (C.PMember expr "arr")
                (valExpr idx)
            TypeApply (Sym ["Tuple"]) ts -> error "TODO"
            TypeApply _ _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.PMember expr "arr")
                (valExpr idx)
            x -> error (show x)


builtinSliceAt :: Value -> Value -> Generate Value
builtinSliceAt val idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    Slice t <- baseTypeOf val
    base <- baseTypeOf t

    case val of
        Ref _ exp -> case base of
            Type.Char -> return $ Ref t $ C.Address $ C.Subscript (C.PMember exp "ptr") (valExpr idx)

            TypeApply (Sym ["Tuple"]) _ -> do
                -- TODO not real tuple case
                let ptr = C.Address $ C.Subscript (C.PMember exp "ptr") (valExpr idx)
                assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

            x -> error (show x)
        Value _ exp -> case base of
            Type.Char -> return $ Ref t $ C.Address $ C.Subscript (C.Member exp "ptr") (valExpr idx)
            TypeApply (Sym ["Tuple"]) ts -> do
                let ptr = C.Address $ C.Subscript (C.Member exp "ptr") (valExpr idx)
                assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

            x -> error (show x)


builtinTableAt :: Value -> Value -> Generate Value
builtinTableAt val idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    TypeApply (Sym ["Table"]) [t] <- baseTypeOf val
    base <- baseTypeOf t
    case val of
        Value _ expr -> case base of
            TypeApply (Sym ["Tuple"]) ts -> do
                -- TODO implement shear
                let ptr = C.Address $ C.Subscript (C.Member expr "r0") (valExpr idx)
                assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

            _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.Member expr "r0")
                (valExpr idx)

            x -> error (show x)

        Ref _ expr -> case base of
            TypeApply (Sym ["Tuple"]) ts -> do
                -- TODO implement shear
                let ptr = C.Address $ C.Subscript (C.PMember expr "r0") (valExpr idx)
                assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

            _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.PMember expr "r0")
                (valExpr idx)

            x -> error (show x)
builtinTableAt _ _ = fail "here"
