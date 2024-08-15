module Builtin where

import Control.Monad
--import Control.Monad.IO.Class

import qualified CAst as C
import Type
import CGenerate
import CBuilder
import Error


builtinLen :: Value -> Generate Value
builtinLen val = case val of
    Ref typ expr -> do
        base <- baseTypeOf typ
        return $ case unfoldType base of
            (Table, [_]) -> Value I64 (C.PMember expr "len")
            (Slice, [_]) -> Value I64 (C.PMember expr "len")
            (Array, [Size n, _]) -> Value I64 (C.Int $ fromIntegral n)

            x -> error (show x)

    Value typ expr -> do
        base <- baseTypeOf typ
        case unfoldType base of
            x -> error (show x)


builtinStore :: Value -> Value -> Generate ()
builtinStore dst@(Ref _ _) src = do
    check (typeof dst == typeof src) "type mismatch"
    base <- baseTypeOf dst
    isCopyable <- isCopyable (typeof dst)

    case src of
        Value _ _ -> case base of
            x | isSimple x -> void $ appendElem $ C.Set (C.Deref $ refExpr dst) (valExpr src)
            x -> error (show x)

        x -> error (show x)


builtinSumEnum :: Value -> Generate Value
builtinSumEnum val = do
    (Sum, _) <- unfoldType <$> baseTypeOf val
    case val of
        Value _ expr -> return $ Value I64 $ C.Member expr "en"
        Ref _ expr   -> return $ Value I64 $ C.PMember expr "en"


builtinSumReset :: Value -> Value -> Generate ()
builtinSumReset sum@(Ref _ _) idx@(Value _ _) = do
    (Sum, _) <- unfoldType <$> baseTypeOf sum
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
    Apply Table t <- baseTypeOf typ
    base <- baseTypeOf t

    let len    = C.PMember expr "len"
    let cap    = C.PMember expr "cap"
    let newLen = (C.Infix C.Plus len $ C.Int 1)
    oldCap <- assign "oldCap" $ Value I64 cap
    
    -- realloc if needed
    if_ (Value Type.Bool $ C.Infix C.GTEq len cap) $ do
        appendElem $ C.Set cap (C.Infix C.Times newLen (C.Int 2))

        -- double mem size
        let pMem = C.PMember expr "r0"
        elemSize <- C.SizeofType <$> cTypeOf t
        let newSize = C.Infix C.Times cap elemSize
        appendElem $ C.Set pMem $ C.Call "GC_realloc" [pMem, newSize]


        case unfoldType base of
            (Tuple, ts) -> do
                cts <- mapM cTypeOf ts
                forM_ (reverse $ zip ts [0..]) $ \(t, i) -> do
                    let ptr = C.Cast (C.Cpointer C.Cvoid) (C.PMember expr "r0")
                    prevSizes <- assign "prev" $ Value I64 $ foldl (C.Infix C.Plus) (C.Int 0) $ map C.SizeofType (take i cts)
                    let src = C.Infix C.Plus ptr (C.Infix C.Times (valExpr oldCap) $ valExpr prevSizes)
                    let dst = C.Infix C.Plus ptr (C.Infix C.Times cap $ valExpr prevSizes)
                    let l = C.Infix C.Times len $ C.SizeofType (cts !! i)
                    appendElem $ C.ExprStmt $ C.Call "memcpy" [dst, src, l]

            (_, _) -> return ()

    void $ appendElem $ C.ExprStmt $ C.Increment $ C.PMember expr "len"



builtinArrayAt :: Value -> Value -> Generate Value
builtinArrayAt value idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    (Array, [Size n, t]) <- unfoldType <$> baseTypeOf value
    case value of -- TODO arrays can also have shear?
        Value _ expr -> makeRef $ Value t $ C.Subscript (C.Member expr "arr") (valExpr idx)
        Ref _ expr   -> makeRef $ Value t $ C.Subscript (C.PMember expr "arr") (valExpr idx)


builtinSliceAt :: Value -> Value -> Generate Value
builtinSliceAt val idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    Apply Slice t <- baseTypeOf val
    base <- baseTypeOf t

    case val of
        Ref _ exp -> case unfoldType base of
            (Tuple, ts) -> assign "ref" $ Ref t $ C.Initialiser [C.PMember exp "ptr", valExpr idx, C.PMember exp "cap" ]
            (_, _)      -> assign "ref" $ Ref t $ C.Address $ C.Subscript (C.PMember exp "ptr") (valExpr idx)

        Value _ exp -> case base of
            Type.Char -> assign "ref" $ Ref t $ C.Address $ C.Subscript (C.Member exp "ptr") (valExpr idx)
            x -> error (show x)


builtinTableAt :: Value -> Value -> Generate Value
builtinTableAt val idx@(Value _ _) = do
    I64 <- baseTypeOf idx
    Apply Table t <- baseTypeOf val
    base <- baseTypeOf t
    case val of
        Value _ expr -> case unfoldType base of
            (Tuple, ts) -> assign "ref" $ Ref t $ C.Initialiser [C.Member expr "r0", valExpr idx, C.Member expr "cap"]
            _           -> assign "ref" $ Ref t $ C.Address $ C.Subscript (C.Member expr "r0") (valExpr idx)

        Ref _ expr -> case unfoldType base of
            (Tuple, ts) -> assign "ref" $ Ref t $ C.Initialiser [C.PMember expr "r0", valExpr idx, C.PMember expr "cap"]
            _           -> assign "ref" $ Ref t $ C.Address $ C.Subscript (C.PMember expr "r0") (valExpr idx)
