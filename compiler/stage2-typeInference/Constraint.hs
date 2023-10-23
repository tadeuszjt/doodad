module Constraint where

import Type

-- constraints obtained from sub-expressions must be to the left
data Constraint
    = ConsEq Type Type
    | ConsBase   Type Type       -- both types must have same base
    | ConsMember Type Int Type   -- t2 is ith elem of t1
    | ConsElem Type Type         -- t2 is elem of t1
    | ConsSubscript Type Type    -- t2 is elem type of t1
    | ConsField Type Int Type 
    | ConsAdtField Type Int Int Type
    | ConsKey Type Type          -- t2 is key type of t1
    | ConsTuple Type [Type]      -- t2s are the member types of 51
    | ConsRecordAccess Type Type
    deriving (Show, Eq, Ord)
