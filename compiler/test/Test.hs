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




test1 = TestCase $ assertEqual "benis" 1 1 

test2 = TestCase $ assertEqual "benis" 2 2

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
        runTestTTAndExit $ TestList [test1, test2, test3 initState]

