module Constraint where

import Type
import Symbol
import Error

-- constraints obtained from sub-expressions must be to the left
data Constraint
    = ConsEq Type Type               -- t1 == t2
    | ConsBase   Type Type           -- base(t1) == base(t2)
    | ConsField Type Int Type        -- ((MyType:t1).x):t2
    | ConsPatTypeField Type Type [Type]  -- 
    | ConsPatField Type Symbol Type
    | ConsForExpr Type Type
    | ConsSlice Type Type           
    | ConsDefault Type Type
    deriving (Eq, Ord)



data ConstraintInfo
    = ConstraintInfo
        { infoTextPos :: TextPos
        , infoMsg     :: String
        }

instance TextPosition ConstraintInfo where
    textPos = infoTextPos
