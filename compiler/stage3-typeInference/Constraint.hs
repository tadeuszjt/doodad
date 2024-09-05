module Constraint where

import Type
import Error

-- constraints obtained from sub-expressions must be to the left
data Constraint
    = ConsEq Type Type               -- t1 == t2
    | ConsDefault Type Type
    deriving (Eq, Ord, Show)


data ConstraintInfo
    = ConstraintInfo
        { infoTextPos :: TextPos
        , infoMsg     :: String
        }

instance TextPosition ConstraintInfo where
    textPos = infoTextPos
