module Numeric.Champernowne.BinaryAdvance
       where

binaryAdvance p f = test 0 upper
  where test a b | b - a <= 1 = b
                 | p (f x)    = test x b
                 | otherwise  = test a x
          where x = (b+a)`div`2
        upper = until (not.p.f) (2*) 1
