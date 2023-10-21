module Main where

import TestTypeMatcher
import TestTypeConstraints

main :: IO ()
main = do
    testTypeMatcher
    testTypeConstraints
