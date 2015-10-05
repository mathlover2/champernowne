module Champernowne
       where

import Math.NumberTheory.Logarithms (integerLogBase)

champernowneDigit base pos
  = if base > pos then pos
    else ((x-1)`div`(base^((-x)`mod`(i+1))*(i+1))+base^((x-1)`mod`(i+1)))
         `mod`base
  where x = pos - l
        l = f (i-1)
        i = binaryAdvanceDownFrom (<=pos) f
            (fromIntegral (integerLogBase base pos)) + 1
        f n = let bnn = base^(n+1)
              in -1 + bnn*(n+1) + (base-bnn)`div`(base-1)

binaryAdvanceDownFrom p f n0
  = let f' n = f (n0-n) in n0 - (binaryAdvance (not . p) f')

binaryAdvance p f = test 0 0
  where test 0 0 | not (p (f 0)) = 0
                 | not (p (f 1)) = 1
                 | otherwise = test 0 1
        test 0 n = (if p (f (2*n))
                    then test 0
                    else test n) (2*n)
        test a b | b - a <= 1 = b
                 | p (f x)    = test x b
                 | otherwise  = test a x
          where x = (b+a)`div`2
