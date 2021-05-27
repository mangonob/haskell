{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List (foldl')
import qualified Data.Vector as V
import System.Environment (getArgs)
import Text.Printf (printf)

data Pair a b = Pair !a !b

main :: IO ()
main = do
  [d] <- map read `fmap` getArgs
  printf "%f\n" $ mean $ V.enumFromTo 1 d

mean :: V.Vector Double -> Double
mean xs = s / fromIntegral n
  where
    Pair n s = V.foldl' k (Pair 0 0) xs
    k (Pair n s) x = Pair (n + 1) (s + x)