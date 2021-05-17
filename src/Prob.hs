module Prob (Prob (..)) where

import Control.Monad
import Data.Ratio

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving (Show)

instance Functor Prob where
  fmap f (Prob xs) = Prob $ fmap (\(v, p) -> (f v, p)) xs

instance Applicative Prob where
  pure x = Prob [(x, 1)]
  Prob fs <*> Prob as =
    Prob [(f a, fp * ap) | (f, fp) <- fs, (a, ap) <- as]

instance Monad Prob where
  return = pure
  Prob xs >>= f = Prob $ concat [[(y, xp * yp) | (y, yp) <- getProb (f x)] | (x, xp) <- xs]

-- Examples
data Coin = Heads | Tails deriving (Eq, Show)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree :: Prob [Coin]
flipThree = sequence [coin, coin, loadedCoin]