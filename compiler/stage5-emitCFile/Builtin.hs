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


builtinTableAppend :: Type -> C.Expression -> Generate ()
builtinTableAppend typ expr = do
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
        appendElem $ C.Set pMem $ C.Call "realloc" [pMem, newSize]

        case unfoldType base of
            (Tuple, ts) -> do
                cts <- mapM cTypeOf ts
                ct <- cTypeOf base
                forM_ (reverse $ zip ts [0..]) $ \(t, i) -> do
                    let ptr = C.Cast (C.Cpointer C.Cvoid) (C.PMember expr "r0")
                    let off = C.Offsetof ct ("m" ++ show i)

                    let src = C.Infix C.Plus ptr (C.Infix C.Times (valExpr oldCap) off)
                    let dst = C.Infix C.Plus ptr (C.Infix C.Times cap off)
                    let l = C.Infix C.Times len $ C.SizeofType (cts !! i)
                    appendElem $ C.ExprStmt $ C.Call "memmove" [dst, src, l]

            (_, _) -> return ()

    void $ appendElem $ C.ExprStmt $ C.Increment $ C.PMember expr "len"



builtinArrayAt :: Type -> C.Expression -> C.Expression -> Generate Value
builtinArrayAt typ cexpr cidx = do
    (Array, [Size n, t]) <- unfoldType <$> baseTypeOf typ
    makeRef $ Value t $ C.Subscript (C.PMember cexpr "arr") cidx


-- TODO slice may represent non-flat memory when representing table row
builtinSliceAt :: Type -> C.Expression -> C.Expression -> Generate Value
builtinSliceAt typ cexpr cidx = do
    Apply Slice t <- baseTypeOf typ
    base <- baseTypeOf t
    -- ptr = ptr + (cap ? 0 : sizeof(struct) * idx)
    -- idx = cap ? idx : 0
    -- cap = cap ? cap : 1
    case unfoldType base of
        (Tuple, ts) -> do 
            ct <- cTypeOf base
            let ptr = C.Infix C.Plus (C.Cast (C.Cpointer C.Cvoid) $ C.Member cexpr "ptr") $ C.CndExpr (C.Member cexpr "cap")
                    (C.Int 0)
                    (C.Infix C.Times cidx (C.SizeofType ct))
            let id = C.CndExpr (C.Member cexpr "cap") cidx (C.Int 0)
            let cap = C.CndExpr (C.Member cexpr "cap") (C.Member cexpr "cap") (C.Int 1)
            assign "ref" $ Ref t $ C.Initialiser [ptr, id, cap]

        (_, _) -> assign "ref" $ Ref t $ C.Address $ C.Subscript (C.Member cexpr "ptr") cidx



builtinTableAt :: Type -> C.Expression -> C.Expression -> Generate Value
builtinTableAt typ cexpr cidx = do
    Apply Table t <- baseTypeOf typ
    base <- baseTypeOf t
    case unfoldType base of
        (Tuple, ts) -> assign "ref" $ Ref t $ C.Initialiser [C.PMember cexpr "r0", cidx, C.PMember cexpr "cap"]
        _           -> assign "ref" $ Ref t $ C.Address $ C.Subscript (C.PMember cexpr "r0") cidx
