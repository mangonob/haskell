{-# LANGUAGE TupleSections #-}

module Crypt.RSA (genKey, encrypt, decrypt) where

import Algo.NumberTheory (exGcd)
import Control.Monad (join)
import Crypt.Prime (isPrime, modPow)
import System.Random (Random (randomR), StdGen, newStdGen)

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

data RSA = RSA {publicKey :: Integer, privateKey :: Integer, decrypt' :: Integer -> Integer, encrypt' :: Integer -> Integer}

instance Show RSA where
  show (RSA e d _ _) = "RSA(publicKey = " ++ show d ++ ", privateKey = " ++ show e ++ ")"

-- createRSA :: Integer -> Integer -> [RSA]
-- Some prime number:
-- 2 3 5 7 11 13 17 19 23 29
-- 31 37 41 43 47 53 59 61 67 71
-- 73 79 83 89 97
-- 101 103 107 109 113
-- 127 131 137 139 149 151 157 163 167 173
-- 179 181 191 193 197 199 211
createRSA :: Integer -> Integer -> [RSA]
createRSA p q =
  let n = p * q
      l = lcm (p - 1) (q - 1)
      es = filter (\e -> gcd e l == 1) [2 .. l -1]
      eds = (\e -> (e,) <$> filter (\d -> e * d `mod` l == 1) [2 .. l -1]) =<< es
      fn p x = x ^ p `mod` n
   in fmap (\(e, d) -> RSA e d (fn e) (fn d)) eds