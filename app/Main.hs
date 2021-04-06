module Main where

import Control.Monad.Writer
import Data.DList

main :: IO ()
main = putStrLn "42"

gcd' :: Int -> Int -> Writer [String] Int
gcd' a 0 = do
  tell ["Finished with " ++ show a]
  return a
gcd' a b = do
  tell [show a ++ " mod " ++ show b ++ " = " ++ show (mod a b)]
  gcd' b (mod a b)
