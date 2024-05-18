module Builtin where

import Control.Monad

import qualified CAst as C
import Type
import CGenerate
import CBuilder
import Error


builtinLen :: Value -> Generate Value
builtinLen val = case val of
    Ref typ expr -> do
        base <- baseTypeOf typ
        return $ case base of
            Apply Table _ -> Value I64 (C.PMember expr "len")
            Apply Slice _ -> Value I64 (C.PMember expr "len")
            x -> error (show x)

    Value typ expr -> do
        base <- baseTypeOf typ
        return $ case base of
            x -> error (show x)
            --TypeApply (Sym ["Table"]) _ -> Value I64 (C.Member expr "len")
            Apply Slice [t]                -> Value I64 (C.Member expr "len")
    --        Type.Array n t -> return $ Value I64 $ C.Int (fromIntegral n)
            _ -> error (show base)


builtinStore :: Value -> Value -> Generate ()
builtinStore dst@(Ref _ _) src = do
    check (typeof dst == typeof src) "type mismatch"
    base <- baseTypeOf dst
    isCopyable <- isCopyable (typeof dst)

    case src of
        Value _ _ -> case base of
            x | isSimple x -> void $ appendElem $ C.Set (C.Deref $ refExpr dst) (valExpr src)
--            TypeApply (Sym ["Tuple"]) ts | isCopyable -> do
--                void $ appendElem $ C.Set (C.Deref $ C.Member (refExpr dst) "ptr") (valExpr src)

            x -> error (show x)

        x -> error (show x)


builtinSumEnum :: Value -> Generate Value
builtinSumEnum val = do
    Apply Sum _ <- baseTypeOf val
    case val of
        Value _ expr -> return $ Value I64 $ C.Member expr "en"
        Ref _ expr   -> return $ Value I64 $ C.PMember expr "en"


builtinSumReset :: Value -> Value -> Generate ()
builtinSumReset sum@(Ref _ _) idx@(Value _ _) = do
    appendElem $ C.ExprStmt $
        C.Call "memset" [ refExpr sum , C.Int 0, C.Sizeof (C.Deref $ refExpr sum) ]
    appendElem $ C.Set (C.PMember (refExpr sum) "en") (valExpr idx)
    return ()


builtinAdd :: Value -> Value -> Generate Value
builtinAdd val1@(Value _ _) val2@(Value _ _) = do
    check (typeof val1 == typeof val2) "type mismatch"
    base <- baseTypeOf val1
    case base of
        x | isInt x || isFloat x || x == Char -> return $ Value (typeof val1) $
            C.Infix C.Plus (valExpr val1) (valExpr val2)

        x -> error (show x)

builtinSubtract :: Value -> Value -> Generate Value
builtinSubtract val1@(Value _ _) val2@(Value _ _) = do
    check (typeof val1 == typeof val2) "type mismatch"
    base <- baseTypeOf val1
    case base of
        x | isInt x || isFloat x || x == Char -> return $ Value (typeof val1) $
            C.Infix C.Minus (valExpr val1) (valExpr val2)
        x -> error (show x)

builtinMultiply :: Value -> Value -> Generate Value
builtinMultiply val1@(Value _ _) val2@(Value _ _) = do
    check (typeof val1 == typeof val2) "type mismatch"
    base <- baseTypeOf val1
    case base of
        x | isInt x || isFloat x || x == Char -> return $ Value (typeof val1) $
            C.Infix C.Times (valExpr val1) (valExpr val2)
        x -> error (show x)


builtinDivide :: Value -> Value -> Generate Value
builtinDivide val1@(Value _ _) val2@(Value _ _) = do
    check (typeof val1 == typeof val2) "type mismatch"
    base <- baseTypeOf val1
    case base of
        x | isInt x || isFloat x || x == Char -> return $ Value (typeof val1) $
            C.Infix C.Divide (valExpr val1) (valExpr val2)
        x -> error (show x)


builtinModulo :: Value -> Value -> Generate Value
builtinModulo val1@(Value _ _) val2@(Value _ _) = do
    check (typeof val1 == typeof val2) "type mismatch"
    base <- baseTypeOf val1
    case base of
        x | isInt x || isFloat x || x == Char -> return $ Value (typeof val1) $
            C.Infix C.Modulo (valExpr val1) (valExpr val2)
        x -> error (show x)


builtinEqual :: Value -> Value -> Generate Value
builtinEqual val1@(Value _ _) val2@(Value _ _) = do
    check (typeof val1 == typeof val2) "type mismatch"
    base <- baseTypeOf val1
    case base of
        x | isInt x || isFloat x || x == Char || x == Bool -> return $ Value Bool $
            C.Infix C.EqEq (valExpr val1) (valExpr val2)


builtinLessThan :: Value -> Value -> Generate Value
builtinLessThan val1@(Value _ _) val2@(Value _ _) = do
    check (typeof val1 == typeof val2) "type mismatch"
    base <- baseTypeOf val1
    case base of
        x | isInt x || isFloat x || x == Char || x == Bool -> return $ Value Bool $
            C.Infix C.LT (valExpr val1) (valExpr val2)


builtinGreaterThan :: Value -> Value -> Generate Value
builtinGreaterThan val1@(Value _ _) val2@(Value _ _) = do
    check (typeof val1 == typeof val2) "type mismatch"
    base <- baseTypeOf val1
    case base of
        x | isInt x || isFloat x || x == Char || x == Bool -> return $ Value Bool $
            C.Infix C.GT (valExpr val1) (valExpr val2)


builtinNot :: Value -> Generate Value
builtinNot val@(Value _ _) = do
    base <- baseTypeOf val
    case base of
        Bool -> return $ Value (typeof val) (C.Not $ valExpr val)


builtinTableAppend :: Value -> Generate ()
builtinTableAppend (Ref typ expr) = do
    Apply Table _ <- baseTypeOf typ

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
    Apply Array [t, Size n] <- baseTypeOf value
    base <- baseTypeOf t

    case value of
        Value _ expr -> case base of
--            TypeApply _ _ -> return $ Ref t $ C.Address $ C.Subscript
--                (C.Member expr "arr")
--                (valExpr idx)
--            TypeApply (Sym ["Tuple"]) _ -> error "TODO"
            x -> error (show x)
        Ref _ expr -> case base of
            x | isSimple x -> return $ Ref t $ C.Address $ C.Subscript
                (C.PMember expr "arr")
                (valExpr idx)
            Apply Tuple ts -> error "TODO"
            Apply _ _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.PMember expr "arr")
                (valExpr idx)
            x -> error (show x)


builtinSliceAt :: Value -> Value -> Generate Value
builtinSliceAt val idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    Apply Slice [t] <- baseTypeOf val
    base <- baseTypeOf t

    case val of
        Ref _ exp -> case base of
            Type.Char -> return $ Ref t $ C.Address $ C.Subscript (C.PMember exp "ptr") (valExpr idx)

            Apply Tuple _ -> do
                -- TODO not real tuple case
                let ptr = C.Address $ C.Subscript (C.PMember exp "ptr") (valExpr idx)
                assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

            x -> error (show x)
        Value _ exp -> case base of
            Type.Char -> return $ Ref t $ C.Address $ C.Subscript (C.Member exp "ptr") (valExpr idx)
            x -> error (show x)
--            TypeApply (Sym ["Tuple"]) ts -> do
--                let ptr = C.Address $ C.Subscript (C.Member exp "ptr") (valExpr idx)
--                assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

            x -> error (show x)


builtinTableAt :: Value -> Value -> Generate Value
builtinTableAt val idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    Apply Table [t] <- baseTypeOf val
    base <- baseTypeOf t
    case val of
        Value _ expr -> case base of
            Apply Tuple ts -> do
                -- TODO implement shear
                let ptr = C.Address $ C.Subscript (C.Member expr "r0") (valExpr idx)
                assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

            _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.Member expr "r0")
                (valExpr idx)

            x -> error (show x)

        Ref _ expr -> case base of
            Apply Tuple ts -> do
                -- TODO implement shear
                let ptr = C.Address $ C.Subscript (C.PMember expr "r0") (valExpr idx)
                assign "ref" $ Ref t $ C.Initialiser [ptr, C.Int 0, C.Int 0]

            _ -> return $ Ref t $ C.Address $ C.Subscript
                (C.PMember expr "r0")
                (valExpr idx)

            x -> error (show x)
builtinTableAt _ _ = fail "here"
