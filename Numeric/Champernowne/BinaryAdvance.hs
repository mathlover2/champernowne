module Numeric.Champernowne.BinaryAdvance
       where

-- binaryAdvance p f = test 0 0
--   where test 0 0 | not (p (f 0)) = 0
--                  | not (p (f 1)) = 1
--                  | otherwise = test 0 1
--         test 0 n = (if p (f (2*n))
--                     then test 0
--                     else test n) (2*n)
--         test a b | b - a <= 1 = b
--                  | p (f x)    = test x b
--                  | otherwise  = test a x
--           where x = (b+a)`div`2

binaryAdvance p f = test 0 upper
  where test a b | b - a <= 1 = b
                 | p (f x)    = test x b
                 | otherwise  = test a x
          where x = (b+a)`div`2
        upper = until (not.p.f) (2*) 1
