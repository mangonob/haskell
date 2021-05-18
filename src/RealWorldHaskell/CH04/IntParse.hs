module RealWorldHaskell.CH04.IntParse () where

import Data.Char (digitToInt)

asInt :: String -> Integer
asInt xs = acc 0 xs

acc :: Integer -> String -> Integer
acc n [] = n
acc n (x : xs) = acc (n * 10 + toInteger (digitToInt x)) xs