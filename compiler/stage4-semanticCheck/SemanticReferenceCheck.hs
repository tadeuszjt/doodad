module SemanticReferenceCheck where

import Control.Monad.Except

import Ir
import Error

-- 1.) references are perishable if containing data structure is modified
-- 2.) ensure references are not used after possible perish
-- 3.) ensure args do not overlap with references


--Just (fromList [])
--tuple::make2{I64, Bool, Tuple{I64, Bool}} [%I64,%Bool]
--        3 = builtin::builtinInit{Tuple{I64, Bool}}({0})    []
--       *4 = builtin::field{0, I64, Tuple{I64, Bool}}(&3)   [4:3]
--        5 = builtin::store{I64}(&4, %1)                    [4:3]
--       *6 = builtin::field{1, Bool, Tuple{I64, Bool}}(&3)  [6:3]
--        7 = builtin::store{Bool}(&6, %2)                   [6:3]
--        return 3

--        2 = builtin::builtinInit{Tuple{I64, Bool}}({0})  
--        3 = builtin::builtinInit{Tuple{I64, Bool}}({0})   
--       *4 = builtin::field{0, I64, Tuple{I64, Bool}}(&3)   
--        5 = builtin::store{Tuple{I64, Bool}}(&3, %2)       -- *4 has perished
--
--       *6 = builtin::field{1, Bool, Tuple{I64, Bool}}(&3)
--        7 = builtin::store{Bool}(&6, %2)                
--
--Just (fromList [])
--container::at{I64, I64, Slice{I64}} [%Slice{I64},%I64]
--        3 = builtin::builtinInit{I64}(0)              
--        4 = convert::convert{I64, I64}(%3)            
--        5 = compare::greaterThanEqual{I64}(%2, %4)    
--        6 = container::len{Slice{I64}}(%1)            
--        7 = compare::lessThan{I64}(%2, %6)
--        8 = boolean::and{Bool}(%5, %7)
--        9 = assert::assert(%8)
--       *10 = builtin::builtinSliceAt{I64}(%1, %2)  [10:1]
--        return 10

semanticReferenceCheck :: FuncIr -> Except Error ()
semanticReferenceCheck funcIr = do
    return ()
