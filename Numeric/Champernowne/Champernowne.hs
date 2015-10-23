module Numeric.Champernowne.Champernowne
       (champernowneDigit
       ,champernowneConstant)
       where

import Numeric.Champernowne.BinaryAdvance
import Math.NumberTheory.Logarithms (integerLogBase)

champernowneDigit base pos
  = if base > pos then pos
    else ((x-1)`div`(base^z*(i+1)) + y)
         `mod`base
  where x = pos - l
        y = if z == i then 1 else 0
        z = (-x) `mod` (i+1)
        l = f (i-1)
        i = binaryAdvanceDownFrom (<=pos) f
            (fromIntegral (integerLogBase base pos)) + 1
        f n = let bnn = base^(n+1)
              in -1 + bnn*(n+1) + (base-bnn)`div`(base-1)

binaryAdvanceDownFrom p f n0
  = let f' n = f (n0-n) in n0 - (binaryAdvance (not . p) f')

champernowneConstant :: (Fractional a) => Integer -> Integer -> a
champernowneConstant base prec
  = let num = foldl1 (\x y -> base*x + y)
              $ map (champernowneDigit base) [0..prec]
    in  (fromIntegral num) / (fromIntegral (base ^ prec))
