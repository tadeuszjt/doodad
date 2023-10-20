module Main where

import qualified Data.Map as Map

import TypeMatcher
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
    assert(typesCouldMatch2 typeFuncs [] type1 type1)                 "type1 == type1"
    assert(typesCouldMatch2 typeFuncs [] type1 (Tuple type1) )        "type1 == ()type1"
    assert(typesCouldMatch2 typeFuncs [] type1 (Tuple (Tuple type1))) "type1 == ()()type1"
    assert(typesCouldMatch2 typeFuncs [] (Tuple type1) (Tuple type1)) "()type1 == ()type1"
    assert(not $ typesCouldMatch2 typeFuncs [] type1 I64)                             "type1 != i64"
    assert(not $ typesCouldMatch2 typeFuncs [] type1 Bool)                            "type1 != bool"
    assert(not $ typesCouldMatch2 typeFuncs [] type1 (Tuple (Record [type1, type1]))) "type1 != bool"


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
    assert(typesCouldMatch2 typeFuncs [] type1 type1)               "type1 == type1"
    assert(not $ typesCouldMatch2 typeFuncs [] type1 (Tuple type1)) "type1 != ()type1"
    assert(not $ typesCouldMatch2 typeFuncs [] type1 (Tuple (Tuple type1))) "type1 != ()()type1"
    assert(typesCouldMatch2 typeFuncs [] (Tuple type1) (Tuple (Tuple type1))) "type1() == ()()type1"
    assert(not $ typesCouldMatch2 typeFuncs [] type1 (Tuple (Record [I64, String]))) "type1 != (i64, string)"


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
    assert(typesCouldMatch2 typeFuncs typeVars type1 type1)               "type1 == type1"
    assert(not $ typesCouldMatch2 typeFuncs typeVars (Tuple type1) type2) "type1 != type2"
    assert(not $ typesCouldMatch2 typeFuncs typeVars (Tuple (Record [type1, String])) (Tuple (Record [I64, String]))) "(type1, string) != (i64, string)"
    assert(typesCouldMatch2 typeFuncs typeVars (Tuple (Record [I64, String])) (Tuple (Record [I64, String]))) "(type1, string) != (i64, string)"
    assert(not $ typesCouldMatch2 typeFuncs typeVars (Tuple (Record [I64, String])) (Tuple type2)) "(type1, string) != ()type2"


-- test3
-- typeDefs:
--  type1[T] = T, type2 = {i64, string}
--
-- test:
--  type1(i64) == type1(i64)
--  type1(i64) != type1(i32)
--  type1(i64) != type1(i64, i64)
--  ()type1(i64) == type1(i64)
test4 :: IO ()
test4 = do
    let tSymbol     = makeSymbol "T" 0
    let type1Symbol = makeSymbol "type1" 0
    let type2Symbol = makeSymbol "type2" 0
    let type1       = TypeApply type1Symbol
    let type2       = TypeApply type2Symbol []
    let typeFuncs   = Map.fromList [ (type1Symbol, ([tSymbol], TypeApply tSymbol [])), (type2Symbol, ([], Record [I64, String])) ]
    let typeVars    = []
    assert(typesCouldMatch2 typeFuncs typeVars (type1 [I64]) (type1 [I64]))  "type1(i64) == type1(i64)"
    assert(not $ typesCouldMatch2 typeFuncs typeVars (type1 [I64]) (type1 [I32]))  "type1(i64) != type1(i32)"
    assert(not $ typesCouldMatch2 typeFuncs typeVars (type1 [I64]) (type1 [I64, I64]))  "type1(i64) != type1(i64, i64)"
    --assert(typesCouldMatch2 typeFuncs typeVars (Tuple (type1 [I64])) (type1 [I64, I64]))  "()type1(i64) != type1(i64, i64)"



main :: IO ()
main = do
    test1
    test2
    test3
    test4
    putStrLn "success"
