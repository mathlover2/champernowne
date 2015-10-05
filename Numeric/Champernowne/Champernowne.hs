module Numeric.Champernowne.Champernowne
       where

import Numeric.Champernowne.BinaryAdvance
import Math.NumberTheory.Logarithms (integerLogBase)

champernowneDigit base pos
  = if base > pos then pos
    else ((x-1)`div`(base^((-x)`mod`(i+1))*(i+1)) + y)
         `mod`base
  where x = pos - l
        y = if (pos - l) `mod` (i + 1) == 1 then 1 else 0
        l = f (i-1)
        i = binaryAdvanceDownFrom (<=pos) f
            (fromIntegral (integerLogBase base pos)) + 1
        f n = let bnn = base^(n+1)
              in -1 + bnn*(n+1) + (base-bnn)`div`(base-1)

binaryAdvanceDownFrom p f n0
  = let f' n = f (n0-n) in n0 - (binaryAdvance (not . p) f')


