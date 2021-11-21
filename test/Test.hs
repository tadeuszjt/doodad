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





test1 = TestCase $ assertEqual "benis" 1 1 

test2 = TestCase $ assertEqual "benis" 2 2

test3 initState = TestCase $ do 
    Right (actual, blocks, defs, state') <- runAll initState testCode
    let expected = ADT []
    assertEqual "type mismatch" expected actual

    where
        testCode :: InsCmp CompileState m => m Type
        testCode = do
            let t = ADT [
                ("a", I64),
                ("b", F32),
                ("s", Typedef "MyADTType")
            ]

            addObj "MyADTType" KeyType $ ObType t Nothing
            pureTypeOf (Typedef "MyADTType")




main = do
    withSession False $ \session -> do
        let initState = initCompileState (JIT.context session) (JIT.dataLayout session) Map.empty "testMod"
        runTestTTAndExit $ TestList [test1, test2, test3 initState]

