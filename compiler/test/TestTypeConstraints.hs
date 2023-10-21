module TestTypeConstraints where

import qualified Data.Map as Map

import Type
import TypeConstraints
import Constraint
import ASTResolved
import Symbol
import Monad
import Error

testTypes :: [Symbol] -> TypeFuncs -> Type -> Type -> [Constraint] -> IO ()
testTypes generics typeFuncs t1 t2 constraints = do
    Right (result, _) <- runBoMT typeFuncs (getConstraintsFromTypes generics t1 t2)
    assert (constraints == result) "invalid constraints"
    


test1 :: IO ()
test1 = do
    let typeFuncs = Map.fromList []
    let generics  = []

    testTypes generics typeFuncs (Type 0) (Type 1)    [ConsEq (Type 0) (Type 1)]
    testTypes generics typeFuncs (Type 1) (Type 1)    []
    testTypes generics typeFuncs (Type 0) I64         [ConsEq (Type 0) I64]
    testTypes generics typeFuncs (Type 0) (Tuple I64) [ConsEq (Type 0) I64]
    testTypes generics typeFuncs String String        []
    --testTypes generics typeFuncs (Tuple $ Type 0) (Tuple I64) [ConsEq (Type 0) I64]



testTypeConstraints :: IO ()
testTypeConstraints = do
    test1
    putStrLn "testTypeConstraints success"
