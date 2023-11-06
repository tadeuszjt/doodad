module Constraint where

import Type
import Symbol

-- constraints obtained from sub-expressions must be to the left
data Constraint
    = ConsEq Type Type
    | ConsBase   Type Type       -- both types must have same base
    | ConsSubscript Type Type    -- t2 is elem type of t1
    | ConsAdtField Type Int Int Type
    | ConsTuple Type [Type]      -- t2s are the member types of 51
    | ConsRecord Type [Type]     -- t2s are the member types of t1
    | ConsRecordAccess Type Type
    | ConsSpecial Type Type
    | ConsField Type Symbol Type
    deriving (Show, Eq, Ord)
