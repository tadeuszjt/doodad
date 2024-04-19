module Constraint where

import Type
import Symbol

-- constraints obtained from sub-expressions must be to the left
data Constraint
    = ConsEq Type Type               -- t1 == t2
    | ConsBase   Type Type           -- base(t1) == base(t2)
    | ConsField Type Int Type        -- ((MyType:t1).x):t2
    | ConsPatField Type Type [Type]  -- 
    | ConsForExpr Type Type
    | ConsCall Type Symbol [Type]   -- symbol(t2s):t1
    deriving (Show, Eq, Ord)
