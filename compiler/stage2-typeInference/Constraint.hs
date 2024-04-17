module Constraint where

import Type
import Symbol

-- constraints obtained from sub-expressions must be to the left
data Constraint
    = ConsEq Type Type
    | ConsBase   Type Type       -- both types must have same base
    | ConsSubscript Type Type    -- t2 is elem type of t1
    | ConsAdtField Type Int [Type]
    | ConsTuple Type [Type]      -- t2s are the member types of 51
    | ConsField Type Symbol Type
    | ConsForExpr Type Type
    | ConsReference Type Type    -- t1 is reference type of t2
    | ConsIdent Type Type        -- t1 is ident type, t2 is exprType
    | ConsBuiltinAt Type Type
    deriving (Show, Eq, Ord)
