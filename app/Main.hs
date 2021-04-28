{-# LANGUAGE ViewPatterns #-}

module Main where

main :: IO ()
main = interact id

np :: (Ord a, Num a) => a -> a -> Maybe a
np k n = if (n >= k) then Just (n - k) else Nothing

fib :: Int -> Integer
fib = (fmap memo [0 ..] !!)
  where
    memo 0 = 1
    memo 1 = 1
    memo (np 2 -> Just n) = fib n + fib (n + 1)
