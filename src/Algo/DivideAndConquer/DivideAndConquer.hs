module Algo.DivideAndConquer.DivideAndConquer where

-- Returns: (Start of maximum sublist, length of maximum sublist)
maximumSublist :: [Integer] -> (Int, Int)
maximumSublist = (\(a, b, _) -> (a, b)) . maximumSublist'

maximumSublist' :: (Ord a, Num a) => [a] -> (Int, Int, a)
maximumSublist' [] = (0, 0, 0)
maximumSublist' [x] = (0, 1, x)
maximumSublist' xs =
  let half = length xs `div` 2
      left = take half xs
      right = drop half xs
      (start, len, s) = maximumSublist' left
      (start', len', s') = maximumSublist' right
      (li, ls) = maximumTail left
      (ri, rs) = maximumTail (reverse right)
   in if s >= s' && s >= ls + rs
        then (start, len, s)
        else
          if s' >= s && s' >= ls + rs
            then (start' + half, len', s')
            else (li, length xs - li - ri, ls + rs)

maximumTail :: (Ord a, Num a) => [a] -> (Int, a)
maximumTail [] = (0, 0)
maximumTail [x] = (0, x)
maximumTail (x : xs)
  | x <= 0 = let (i, s) = maximumTail xs in (i + 1, s)
  | otherwise =
    let a = sum (x : xs)
        (i, b) = maximumTail xs
     in if a >= b
          then (0, a)
          else (i + 1, b)