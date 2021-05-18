module RealWorldHaskell.CH03.Chore () where

import Data.List

myNot :: Bool -> Bool
myNot True = False
myNot False = True

sumList :: Num p => [p] -> p
sumList [] = 0
sumList (x : xs) = x + sumList xs

third :: (a, b, c) -> c
third (_, _, c) = c

length' :: Num p => [a] -> p
length' [] = 0
length' (x : xs) = length' xs + 1

-- 创建回文（偶）
palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x : xs) = x : palindrome xs ++ [x]

-- 判断是否是回文
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (x : []) = True
isPalindrome (x : xs) = x == last xs && isPalindrome (init xs)

sortByLength :: Foldable t => [t a] -> [t a]
sortByLength xs = sortOn length xs

intersperse' :: a -> [[a]] -> [a]
intersperse' x [] = []
intersperse' x (y : []) = y
intersperse' x (y : ys) = y ++ [x] ++ intersperse' x ys