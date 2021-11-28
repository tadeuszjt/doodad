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
import Compile
import qualified AST as S

import LLVM.AST.Global
import LLVM.AST hiding (Type)


noPos = TextPos 0 0 0 0


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


instrTest initState = TestCase $ do

    blocks <- run $ cmpStmt (S.Return noPos Nothing)
    let block0 = BasicBlock (UnName 0) [] $ Do (Ret Nothing [])
    let block1 = BasicBlock (UnName 1) [] $ Do (Ret Nothing [])
    assertEqual "result" blocks [block0, block1]

    where
        run :: InstrCmpT CompileState IO () -> IO [BasicBlock]
        run f = do
            Right (_, blocks, _, _) <- runAll initState f
            return blocks



main = do
    withSession False $ \session -> do
        let initState = initCompileState (JIT.context session) (JIT.dataLayout session) Map.empty "testMod"
        runTestTTAndExit $
            TestList
                [ typeTest
                , instrTest initState
                ]

