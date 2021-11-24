{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.Map as Map

import Test.HUnit
import System.Exit
import Type
import Monad
import JIT
import State
import Value
import Error

import LLVM.AST.Global
import LLVM.AST hiding (Type)



typeTest = TestCase $ do
    -- must have empty array
    assertEqual "adt" False $ isEmptyADT I64
    assertEqual "adt" False $ isEmptyADT $ ADT [("", I64)]
    assertEqual "adt" True $ isEmptyADT $ ADT []

    -- must have exactly one field which is non-Void 
    assertEqual "adt" False $ isPtrADT $ I32
    assertEqual "adt" False $ isPtrADT $ ADT []
    assertEqual "adt" True $ isPtrADT $ ADT [("", I64)]
    assertEqual "adt" False $ isPtrADT $ ADT [("sym", Void)]
    assertEqual "adt" False $ isPtrADT $ ADT [("", Void), ("", I64)]

    -- must have at least one field and all void
    assertEqual "adt" False $ isEnumADT $ I64
    assertEqual "adt" False $ isEnumADT $ ADT []
    assertEqual "adt" False $ isEnumADT $ ADT [("", I64)]
    assertEqual "adt" True $ isEnumADT $ ADT [("sym", Void)]
    assertEqual "adt" True $ isEnumADT $ ADT [("a", Void), ("b", Void)]
    assertEqual "adt" False $ isEnumADT $ ADT [("a", Void), ("b", I64)]

    -- must have at least two fields and at least one non-void field
    assertEqual "adt" False $ isNormalADT $ I64
    assertEqual "adt" False $ isNormalADT $ ADT []
    assertEqual "adt" False $ isNormalADT $ ADT [("", I64)]
    assertEqual "adt" False $ isNormalADT $ ADT [("sym", Void)]
    assertEqual "adt" False $ isNormalADT $ ADT [("a", Void), ("b", Void)]
    assertEqual "adt" True $ isNormalADT $ ADT [("a", Void), ("b", I64)]
    assertEqual "adt" True $ isNormalADT $ ADT [("a", F32), ("b", Void), ("", I32)]


test3 initState = TestCase $ do 
    runTypeTest (pureTypeOf I64) $ I64
    runTypeTest (pureTypeOf Void) $ Void

    runTypeTest (pureTypeOf $ Tuple [("", I64)])              $ Tuple [("", I64)]
    runTypeTest (pureTypeOf $ Tuple [("a", I64), ("b", I32)]) $ Tuple [("", I64), ("", I32)]

    runTypeTest (pureTypeOf $ Table [Char]) $ Table [Char]

    runTypeTest testCode1 $ ADT [("a", I64), ("b", F32), ("s", Self) ]
    where
        runTypeTest :: InstrCmpT CompileState IO Type -> Type -> IO ()
        runTypeTest f expected = do
            Right (actual, _, _, _) <- runAll initState f
            assertEqual "type mismatch" expected actual

        testCode1 :: InsCmp CompileState m => m Type
        testCode1 = do
            let t = ADT [ ("a", I64), ("b", F32), ("s", Typedef "MyADTType") ]
            addObj "MyADTType" KeyType (ObType t Nothing)
            pureTypeOf (Typedef "MyADTType")



main = do
    withSession False $ \session -> do
        let initState = initCompileState (JIT.context session) (JIT.dataLayout session) Map.empty "testMod"
        runTestTTAndExit $ TestList [typeTest, test3 initState]

