module RealWorldHaskell.CH04.Chore () where

import Data.Char

upperCase :: [Char] -> [Char]
upperCase = map toUpper

lowerCase :: [Char] -> [Char]
lowerCase = map toLower

titleCase :: [Char] -> [Char]
titleCase [] = []
titleCase (x : xs) = toUpper x : lowerCase xs

sum' :: (Foldable t, Num b) => t b -> b
sum' = foldl (+) 0

prod' :: (Foldable t, Num b) => t b -> b
prod' = foldl (*) 1

asInt :: String -> Either String Integer
asInt xs = foldl step (Right 0) xs
  where
    step (Left a) _ = Left a
    step (Right n) x
      | isDigit x = Right (n * 10 + toInteger (digitToInt x))
      | otherwise = Left (show x ++ " is not digit")