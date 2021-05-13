{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Others.AIAudit () where

import Algo
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import System.Random

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

withoutLog :: Writer b a -> a
withoutLog w = fst $ runWriter w

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)

gcd' :: Integer -> Integer -> Writer [String] Integer
gcd' a 0 = writer (a, ["Finish with " ++ show a])
gcd' a b = do
  tell ["Calculate gcd of " ++ show a ++ " " ++ show b]
  gcd' b (mod a b)

applicative :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
applicative f g = \x -> f x (g x)

monad :: (t1 -> t2) -> (t2 -> t1 -> t3) -> t1 -> t3
monad h f = \w -> f (h w) w

twice :: Reader String [String]
twice = do
  a <- ask
  return [a, a]

_in x y z = z > x && z <= y

prdBad :: ([(Double, Double)], Double)
prdBad = ([(500000.0, 0.0), (1000000, 0.000080), (3000000, 0.000060), (5000000, 0.00040)], 1000.0)

prd1 :: ([(Double, Double)], Double)
prd1 = ([(1000000.0, 0.015), (3000000, 0.01), (5000000, 0.005)], 1000.0)

prd2 :: ([(Double, Double)], Double)
prd2 = ([(500000.0, 0.001), (1000000, 0.0005)], 1000.0)

prd3 :: ([(Double, Double)], Double)
prd3 = ([(1000000000000.0, 0)], 1000.0)

prd4 :: ([(Double, Double)], Double)
prd4 = ([(200000.0, 0.0012), (500000, 0.0008), (5000000, 0.005)], 1000.0)

prd5 :: ([(Double, Double)], Double)
prd5 = ([(500000.0, 0.0100), (1000000, 0.0080), (3000000, 0.0060), (5000000, 0.0040)], 1000.0)

prd6 :: ([(Double, Double)], Double)
prd6 = ([(1000000.0, 0.015), (3000000, 0.01), (5000000, 0.008)], 1000.0)

prd7 :: ([(Double, Double)], Double)
prd7 = ([(1000000.0, 0.0120), (2000000, 0.0060), (5000000, 0.0020)], 1000.0)

prd8 :: ([(Double, Double)], Double)
prd8 = ([(500000.0, 0.015), (2000000, 0.01), (5000000, 0.005)], 1000.0)

prd9 :: ([(Double, Double)], Double)
prd9 = ([(1000000.0, 0.015), (3000000, 0.009), (5000000, 0.003)], 1000.0)

prd10 :: ([(Double, Double)], Double)
prd10 = ([(1000000.0, 0.0012), (5000000, 0.0008)], 1000.0)

rateOf :: Ord t => [(t, a)] -> t -> Maybe a
rateOf [] _ = Nothing
rateOf ((x, r) : xs) ((<= x) -> True) = Just r
rateOf (x : xs) y = rateOf xs y

calFee (rs, m) (rateOf rs -> Nothing) = m
calFee (rs, m) y@(rateOf rs -> Just r) = y * r / (r + 1)

generatePrd :: ([(Double, Double)], Double) -> IO [[Double]]
generatePrd prd = do
  gen <- newStdGen
  let dataSet = generateDataSet nGaussian gen [0, 50000 .. 10000000]
  return $ fmap (\x -> [x, calFee prd x]) dataSet

calPrdRates prd xs = fmap (\x -> [x, calFee prd x]) xs

gaussian :: Floating a => a -> a -> a -> a
gaussian u s x = 1 / sqrt (2 * pi) * exp (- (x - u) ^ 2 / (2 * s ^ 2))

shuffle :: (Num a, Random a, RandomGen t) => (a, a) -> t -> [a] -> [a]
shuffle _ _ [] = []
shuffle (a, b) gen (x : xs) =
  let (rate, nextGen) = randomR (a, b) gen
   in rate * x : shuffle (a, b) nextGen xs

dump :: (RandomGen g) => Double -> g -> [a] -> [a]
dump r gen [] = []
dump r gen (x : xs) =
  let (z, nextGen) = randomR (0, 1.0) gen
      ys = dump r nextGen xs
   in if z <= r then x : ys else ys

pick c xs = do
  gen <- newStdGen
  return $ dump (c / (fromIntegral $ length xs)) gen xs

generateDataSet :: RandomGen g => (Double -> Double) -> g -> [Double] -> [Double]
generateDataSet _ gen [] = []
generateDataSet f gen (x : xs) =
  let (r, nextGen) = randomR (0, 1.0) gen
      ys = generateDataSet f nextGen xs
   in if (r <= f x) then x : ys else ys

histogram :: Double -> Double -> [Double] -> [[Double]]
histogram s n [] = []
histogram s n (y : ys) =
  let (inRange, outRange) = span (<= (s * n)) ys
   in [s * n, fromIntegral (length inRange)] : histogram s (n + 1) outRange

nGaussian = gaussian 0 1

normalGenerate = individualGenerate

individualGenerate gen = generateDataSet (gaussian 400000 2000000) gen [1, 100 .. 10000000]

organizationGenerate gen = generateDataSet (gaussian 6000000 2000000) gen [1, 100 .. 10000000]

dangerGenerate gen = generateDataSet (gaussian 5000000 200000) gen [1, 100 .. 10000000]

createExample = do
  gen <- newStdGen
  let dataSet = normalGenerate gen
  return $ histogram 100000 1 dataSet

createRatesExample prd = do
  gen <- newStdGen
  let dataSet = normalGenerate gen
  nGen <- newStdGen
  let (count, _) = randomR (250, 300) nGen
  pick count $ fmap (\x -> [x, calFee prd x]) dataSet

createShareFeeExample prd nav = do
  gen <- newStdGen
  let dataSet = normalGenerate gen
  nGen <- newStdGen
  let (count, _) = randomR (250, 300) nGen
  pick count $ fmap (\x -> [x, x * nav, calFee prd x]) dataSet

createFeeExample prd = do
  gen <- newStdGen
  let dataSet = normalGenerate gen
  let fees = sort $ fmap (calFee prd1) dataSet
  return $ histogram 1000 1 fees

filterHistogram :: (RandomGen g) => Double -> g -> [[Double]] -> [[Double]]
filterHistogram r gen [] = []
filterHistogram r gen ((x : y : t) : xs) =
  let (z, nextGen) = randomR (0, 1.0) gen
      ys = filterHistogram r nextGen xs
   in if z <= r then ((x : y : t) : ys) else ((x : 0.0 : t) : ys)

createAmountExample = do
  gen <- newStdGen
  let dataSet = normalGenerate gen
  dumpG <- newStdGen
  return $ filterHistogram 0.8 dumpG $ histogram 100000 1 dataSet

createFeeShare nav = do
  gen <- newStdGen
  let dataSet = normalGenerate gen
  nGen <- newStdGen
  let (count, _) = randomR (250, 300) nGen
  pick count $ fmap (\x -> [x / nav, x]) dataSet

createShareExample nav = do
  gen <- newStdGen
  let dataSet = normalGenerate gen
  dumpG <- newStdGen
  return $ filterHistogram 0.8 dumpG $ histogram 100000 1 $ fmap (/ nav) dataSet