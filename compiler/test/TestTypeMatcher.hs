module TestTypeMatcher where

import qualified Data.Map as Map
import Control.Monad

import Type
import Symbol

makeSymbol :: String -> Int -> Symbol
makeSymbol sym n = SymResolved "main" sym n

check :: Bool -> String -> IO ()
check cnd str = do
    unless cnd (error str)


run = runTypeDefsMonad

-- test1
-- typeDefs:
--  type1 = i64
--
-- test:
--  type1   == type1
--  type1   == ()type1
--  type1   == ()()type1
--  ()type1 == ()type1
--
--  type1   != i64
--  type1   != bool
--  type1   != (type1, type1)
test1 :: IO ()
test1 = do
    let type1Symbol = makeSymbol "type1" 0
    let type1       = TypeApply type1Symbol []
    let typeFuncs = Map.fromList [ (type1Symbol, ([], I64)) ]
    check(run typeFuncs (typesCouldMatch [] type1 type1)) "type1 == type1"
    check(run typeFuncs $ typesCouldMatch [] type1 (Tuple type1) )        "type1 == ()type1"
    check(run typeFuncs $ typesCouldMatch [] type1 (Tuple (Tuple type1))) "type1 == ()()type1"
    check(run typeFuncs $ typesCouldMatch [] (Tuple type1) (Tuple type1)) "()type1 == ()type1"
    check(not $ run typeFuncs $ typesCouldMatch [] type1 I64)                             "type1 != i64"
    check(not $ run typeFuncs $ typesCouldMatch [] type1 Bool)                            "type1 != bool"
    check(not $ run typeFuncs $ typesCouldMatch [] type1 (Tuple (Record [type1, type1]))) "type1 != bool"

-- test2
-- typeDefs:
--  type1 = {i64, string}
--
-- test:
--  type1   == type1
--  type1   != ()type1
--  type1   != ()()type1
--  ()type1 == ()()type1
--  type1   != (i64, string)
test2 :: IO ()
test2 = do
    let type1Symbol = makeSymbol "type1" 0
    let type1       = TypeApply type1Symbol []
    let typeFuncs = Map.fromList [ (type1Symbol, ([], Record [I64, String])) ]
    check(run typeFuncs $ typesCouldMatch [] type1 type1)               "type1 == type1"
    check(not $ run typeFuncs $ typesCouldMatch [] type1 (Tuple type1)) "type1 != ()type1"
    check(not $ run typeFuncs $ typesCouldMatch [] type1 (Tuple (Tuple type1))) "type1 != ()()type1"
    check(run typeFuncs $ typesCouldMatch [] (Tuple type1) (Tuple (Tuple type1))) "type1() == ()()type1"
    check(not $ run typeFuncs $ typesCouldMatch [] type1 (Tuple (Record [I64, String]))) "type1 != (i64, string)"


-- test3
-- typeDefs:
--  type1 = i64, type2 = {i64, string}
--
-- test:
--  type1  == type1
--  type1  != type2
--  (type1, string) != (i64, string)
--  (i64, string) == (i64, string)
--  (i64, string) != ()type2
test3 :: IO ()
test3 = do
    let type1Symbol = makeSymbol "type1" 0
    let type2Symbol = makeSymbol "type2" 0
    let type1       = TypeApply type1Symbol []
    let type2       = TypeApply type2Symbol []
    let typeFuncs   = Map.fromList [ (type1Symbol, ([], I64)), (type2Symbol, ([], Record [I64, String])) ]
    let generics    = []
    check(run typeFuncs $ typesCouldMatch generics type1 type1)               "type1 == type1"
    check(not $ run typeFuncs $ typesCouldMatch generics (Tuple type1) type2) "type1 != type2"
    check(not $ run typeFuncs $ typesCouldMatch generics (Tuple (Record [type1, String])) (Tuple (Record [I64, String]))) "(type1, string) != (i64, string)"
    check(run typeFuncs $ typesCouldMatch generics (Tuple (Record [I64, String])) (Tuple (Record [I64, String]))) "(type1, string) != (i64, string)"
    check(not $ run typeFuncs $ typesCouldMatch generics (Tuple (Record [I64, String])) (Tuple type2)) "(type1, string) != ()type2"


-- test4
-- typeDefs:
--  type1[T] = T, type2 = {i64, string}
--
-- test:
--  type1(i64) == type1(i64)
--  type1(i64) != type1(i32)
--  type1(i64) != type1(i64, i64)
--  ()type1(i64) == type1(i64)
--  type1(type2) != ()type1(type2)
test4 :: IO ()
test4 = do
    let aTuple      = Tuple (Record [I8, Bool])
    let tSymbol     = makeSymbol "T" 0
    let type1Symbol = makeSymbol "type1" 0
    let type2Symbol = makeSymbol "type2" 0
    let type1       = TypeApply type1Symbol
    let type2       = TypeApply type2Symbol []
    let typeFuncs   = Map.fromList [ (type1Symbol, ([tSymbol], TypeApply tSymbol [])), (type2Symbol, ([], Record [I64, String])) ]
    let generics    = []
    check(run typeFuncs $ typesCouldMatch generics (type1 [I64]) (type1 [I64]))                    "type1(i64) == type1(i64)"
    check(not $ run typeFuncs $ typesCouldMatch generics (type1 [I64]) (type1 [I32]))              "type1(i64) != type1(i32)"
    check(run typeFuncs $ typesCouldMatch generics (Tuple (type1 [I64])) (type1 [I64]))            "()type1(i64) == type1(i64)"
    check(not $ run typeFuncs $ typesCouldMatch generics (type1 [type2]) (Tuple (type1 [type2])))  "type1(type2) != ()type1(type2)"

-- test5
-- generics: T, G
-- typeDefs:
--
-- test:
--  i64 == T
--  T == i8
--  T == G
--  ()T == T
test5 :: IO ()
test5 = do
    let tSymbol = makeSymbol "T" 0
    let gSymbol = makeSymbol "G" 0
    let typeFuncs   = Map.fromList []
    let generics    = [tSymbol, gSymbol]
    check(run typeFuncs $ typesCouldMatch generics I64 (TypeApply tSymbol []))                            "i64 == T"
    check(run typeFuncs $ typesCouldMatch generics (TypeApply tSymbol []) I8)                             "T == i8"
    check(run typeFuncs $ typesCouldMatch generics (TypeApply tSymbol []) (TypeApply gSymbol []))         "T == G"
    check(run typeFuncs $ typesCouldMatch generics (Tuple $ TypeApply tSymbol []) (TypeApply tSymbol [])) "()T == T"
    check(run typeFuncs $ typesCouldMatch generics I64 (Tuple $ TypeApply tSymbol []))                    "i64 == ()T"


test6 :: IO ()
test6 = do
    let tSymbol = makeSymbol "T" 0
    let gSymbol = makeSymbol "G" 0
    let kSymbol = makeSymbol "Key" 0
    let generics = [gSymbol]
    let typeFuncs = Map.fromList [ (kSymbol, ([tSymbol], I64)) ]
    let gApply = TypeApply gSymbol []

    check(run typeFuncs $ typesCouldMatch generics (TypeApply kSymbol [I64]) (TypeApply kSymbol [gApply])) "Key(i64) == Key(G)"
    check(run typeFuncs $ typesCouldMatch generics (TypeApply kSymbol [gApply]) (TypeApply kSymbol [I64])) "Key(G) == Key(i64)"

test7 :: IO ()
test7 = do
    let gSymbol = makeSymbol "G" 0
    let generics = [gSymbol]
    let typeFuncs = Map.fromList []
    let gApply = TypeApply gSymbol []

    check(run typeFuncs $ typesCouldMatch generics (Record [I64]) (RecordApply gApply)) "{I64} == {}G"

testTypeMatcher :: IO ()
testTypeMatcher = do
    test1
    test2
    test3
    test4
    test5
    test6
    test7
    putStrLn "testTypeMatcher success"
