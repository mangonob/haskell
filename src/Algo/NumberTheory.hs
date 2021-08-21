module Algo.NumberTheory (exGcd) where

exEuclid :: Integral c => c -> c -> (c, c, c)
exEuclid a 0 = (a, 1, 0)
exEuclid a b =
  let (d, x, y) = exEuclid b (mod a b)
   in (d, y, x - div a b * y)

exGcd :: Integer -> Integer -> (Integer, Integer, Integer)
exGcd = exEuclid