
module Laziness where

import Debug.Trace

testLazy b = let x = trace "Calculating." 1 + 1
             in if b then x else 1
