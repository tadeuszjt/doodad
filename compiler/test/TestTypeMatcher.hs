module TestTypeMatcher where

import qualified Data.Map as Map

import Type
import Symbol
import Error

makeSymbol :: String -> Int -> Symbol
makeSymbol sym n = SymResolved "main" sym n


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
    assert(typesCouldMatch typeFuncs [] type1 type1)                 "type1 == type1"
    assert(typesCouldMatch typeFuncs [] type1 (Tuple type1) )        "type1 == ()type1"
    assert(typesCouldMatch typeFuncs [] type1 (Tuple (Tuple type1))) "type1 == ()()type1"
    assert(typesCouldMatch typeFuncs [] (Tuple type1) (Tuple type1)) "()type1 == ()type1"
    assert(not $ typesCouldMatch typeFuncs [] type1 I64)                             "type1 != i64"
    assert(not $ typesCouldMatch typeFuncs [] type1 Bool)                            "type1 != bool"
    assert(not $ typesCouldMatch typeFuncs [] type1 (Tuple (Record [type1, type1]))) "type1 != bool"


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
    assert(typesCouldMatch typeFuncs [] type1 type1)               "type1 == type1"
    assert(not $ typesCouldMatch typeFuncs [] type1 (Tuple type1)) "type1 != ()type1"
    assert(not $ typesCouldMatch typeFuncs [] type1 (Tuple (Tuple type1))) "type1 != ()()type1"
    assert(typesCouldMatch typeFuncs [] (Tuple type1) (Tuple (Tuple type1))) "type1() == ()()type1"
    assert(not $ typesCouldMatch typeFuncs [] type1 (Tuple (Record [I64, String]))) "type1 != (i64, string)"


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
    let typeVars    = []
    assert(typesCouldMatch typeFuncs typeVars type1 type1)               "type1 == type1"
    assert(not $ typesCouldMatch typeFuncs typeVars (Tuple type1) type2) "type1 != type2"
    assert(not $ typesCouldMatch typeFuncs typeVars (Tuple (Record [type1, String])) (Tuple (Record [I64, String]))) "(type1, string) != (i64, string)"
    assert(typesCouldMatch typeFuncs typeVars (Tuple (Record [I64, String])) (Tuple (Record [I64, String]))) "(type1, string) != (i64, string)"
    assert(not $ typesCouldMatch typeFuncs typeVars (Tuple (Record [I64, String])) (Tuple type2)) "(type1, string) != ()type2"


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
    let typeVars    = []
    assert(typesCouldMatch typeFuncs typeVars (type1 [I64]) (type1 [I64]))                    "type1(i64) == type1(i64)"
    assert(not $ typesCouldMatch typeFuncs typeVars (type1 [I64]) (type1 [I32]))              "type1(i64) != type1(i32)"
    assert(typesCouldMatch typeFuncs typeVars (Tuple (type1 [I64])) (type1 [I64]))            "()type1(i64) == type1(i64)"
    assert(not $ typesCouldMatch typeFuncs typeVars (type1 [type2]) (Tuple (type1 [type2])))  "type1(type2) != ()type1(type2)"

-- test5
-- typeVars: T, G
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
    let typeVars    = [tSymbol, gSymbol]
    assert(typesCouldMatch typeFuncs typeVars I64 (TypeApply tSymbol []))                            "i64 == T"
    assert(typesCouldMatch typeFuncs typeVars (TypeApply tSymbol []) I8)                             "T == i8"
    assert(typesCouldMatch typeFuncs typeVars (TypeApply tSymbol []) (TypeApply gSymbol []))         "T == G"
    assert(typesCouldMatch typeFuncs typeVars (Tuple $ TypeApply tSymbol []) (TypeApply tSymbol [])) "()T == T"
    assert(typesCouldMatch typeFuncs typeVars I64 (Tuple $ TypeApply tSymbol []))                    "i64 == ()T"


testTypeMatcher :: IO ()
testTypeMatcher = do
    test1
    test2
    test3
    test4
    test5
    putStrLn "testTypeMatcher success"