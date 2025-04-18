module Builtin where

import Control.Monad

import qualified CAst as C
import Type
import CGenerate
import CBuilder


builtinSumReset :: Type.Type -> C.Expression -> C.Expression -> Generate ()
builtinSumReset sumType sum idx = do
    (Sum, _) <- unfoldType <$> baseTypeOf sumType
    appendElem $ C.ExprStmt $
        C.Call "memset" [ sum , C.Int 0, C.Sizeof (C.Deref sum) ]
    void $ appendElem $ C.Set (C.PMember sum "en") idx


builtinTableAppend :: Type -> C.Expression -> Generate ()
builtinTableAppend typ expr = do
    --appendElem $ C.ExprStmt $ C.Call "puts" [C.String "builtinTableAppend"]

    Apply Table t <- baseTypeOf typ
    base <- baseTypeOf t

    let len    = C.PMember expr "len"
    let cap    = C.PMember expr "cap"
    let newLen = (C.Infix C.Plus len $ C.Int 1)
    oldCap <- assignVal "oldCap" I64 cap
    
    -- realloc if needed
    if_ (C.Infix C.GTEq len cap) $ do
        appendElem $ C.Set cap (C.Infix C.Times newLen (C.Int 2))

        -- double mem size
        let pMem = C.PMember expr "r0"
        elemSize <- C.SizeofType <$> cTypeOf t
        let newSize = C.Infix C.Times cap elemSize
        appendElem $ C.Set pMem $ C.Call "realloc" [pMem, newSize]
        appendElem $ C.ExprStmt $ C.Call "assert" [pMem]

        -- zero memory
        let oldSize = C.Infix C.Times (oldCap) elemSize
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

                    let src = C.Infix C.Plus ptr (C.Infix C.Times (oldCap) off)
                    let dst = C.Infix C.Plus ptr (C.Infix C.Times cap off)
                    let l = C.Infix C.Times len $ C.SizeofType (cts !! i)
                    appendElem $ C.ExprStmt $ C.Call "memmove" [dst, src, l]
                    appendElem $ C.ExprStmt $ C.Call "memset"  [src, C.Int 0, C.Infix C.Minus dst src]

            (_, _) -> return ()

    void $ appendElem $ C.ExprStmt $ C.Increment $ C.PMember expr "len"
