module Main where

import System.Random

main = do
  gen <- newStdGen
  putStrLn $ take 20 $ randomRs ('a', 'z') gen

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (first, newGen) = random gen
      (second, newGen') = random newGen
      (third, _) = random newGen'
   in (first, second, third)

finiteRandoms :: (Integral a1, Random a2, RandomGen b, Num a1) => a1 -> b -> ([a2], b)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (r, gen') = random gen
      (xs, gen'') = finiteRandoms (n - 1) gen'
   in (r : xs, gen'')