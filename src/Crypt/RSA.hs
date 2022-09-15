module Crypt.RSA (genKey, encrypt, decrypt) where

import Algo.NumberTheory
import Crypt.Prime
import System.Random

data Key = Key Integer Integer deriving (Show)

type KeyPair = (Key, Key)

bitBounds :: (Integral a) => a -> (Integer, Integer)
bitBounds x = (2 ^ x, 2 ^ (x + 1))

defaultBounds :: (Integer, Integer)
defaultBounds = bitBounds 512

genKey :: IO KeyPair
genKey = do
  gen <- newStdGen
  let (p, genQ) = genPrime defaultBounds gen
  let (q, genE) = genPrime defaultBounds genQ
  let n = p * q
  let l = lcm (p - 1) (q - 1)
  let (e, genD) = genR (\x -> gcd x l == 1) (2, l - 1) genE
  let (_, d, _) = exGcd e l
  return (Key n e, Key n (mod (d + l) l))

-- Gen prime number in range
genPrime :: (Integer, Integer) -> StdGen -> (Integer, StdGen)
genPrime = genR isPrime

-- Gen number in range (filter by predicate)
genR :: (Integer -> Bool) -> (Integer, Integer) -> StdGen -> (Integer, StdGen)
genR f bs gen =
  let (p, nextGen) = randomR bs gen
   in if f p
        then (p, nextGen)
        else genR f bs nextGen

encrypt :: Key -> Integer -> Integer
encrypt (Key n e) = modPow n e

decrypt :: Key -> Integer -> Integer
decrypt (Key n d) = modPow n d