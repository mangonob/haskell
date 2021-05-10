module Main where

import Algo
import Control.Monad.Reader
import Control.Monad.Writer

main :: IO ()
main = interact id

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