module Algo.Sort.Sort (insertSort) where

import Data.DList (fromList, toList)

-- Insert sort
insertSort :: Ord t => [t] -> [t]
insertSort = foldr insert []

insert :: Ord t => t -> [t] -> [t]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

-- Bubble sort
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = toList $ fromList (bubbleSort (init (bubble xs))) `mappend` fromList [last (bubble xs)]

-- Bubble the maximum value to the end
bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x : y : xs)
  | x > y = y : bubble (x : xs)
  | otherwise = x : bubble (y : xs)

-- Merge sort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let half = length xs `div` 2
   in merge (mergeSort (take half xs)) (mergeSort (drop half xs))

-- Merge two sorted list into one list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- Awesome quick sort
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x : xs) = quickSort [y | y <- xs, y < x] ++ x : quickSort [y | y <- xs, y >= x]