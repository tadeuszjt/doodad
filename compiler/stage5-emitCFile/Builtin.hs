module Builtin where

import Control.Monad
--import Control.Monad.IO.Class

import qualified CAst as C
import Type
import CGenerate
import CBuilder


builtinSumReset :: Value -> Value -> Generate ()
builtinSumReset sum@(Ref _ _) idx@(Value _ _) = do
    (Sum, _) <- unfoldType <$> baseTypeOf sum
    appendElem $ C.ExprStmt $
        C.Call "memset" [ refExpr sum , C.Int 0, C.Sizeof (C.Deref $ refExpr sum) ]
    appendElem $ C.Set (C.PMember (refExpr sum) "en") (valExpr idx)
    return ()


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
        appendElem $ C.ExprStmt $ C.Call "assert" [pMem]

        -- zero memory
        let oldSize = C.Infix C.Times (valExpr oldCap) elemSize
        let zeroSize = C.Infix C.Minus newSize oldSize
        let zeroPtr  = C.Infix C.Plus (C.Cast (C.Cpointer C.Cvoid) pMem) oldSize
        appendElem $ C.ExprStmt $ C.Call "memset" [zeroPtr, C.Int 0, zeroSize]

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
