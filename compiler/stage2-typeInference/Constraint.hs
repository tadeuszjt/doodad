module Constraint where

import Type
import Symbol

-- constraints obtained from sub-expressions must be to the left
data Constraint
    = ConsEq Type Type
    | ConsBase   Type Type          -- both types must have same base
    | ConsField Type Symbol Type
    | ConsAdtField Type Int [Type]
    | ConsForExpr Type Type
    deriving (Show, Eq, Ord)
