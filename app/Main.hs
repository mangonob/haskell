module Main where

main :: IO ()
main = print (length' "mangonob")

fabonacii :: (Integral a) => a -> a
fabonacii 0 = 1
fabonacii 1 = 1
fabonacii x
  | x < 0 = 0
  | x > 0 = fabonacii (x - 1) + fabonacii (x - 2)

length' :: [a] -> Integer
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: Num p => [p] -> p
sum' [] = 0
sum' (x : xs) = x + sum' xs

max' :: Ord a => [a] -> a
max' [] = error "Maximum of empty list"
max' [x] = x
max' (x : xs)
  | x > maxOfTail = x
  | otherwise = maxOfTail
  where
    maxOfTail = max' xs

maximum' :: Ord a => [a] -> a
maximum' [] = error "Maximum of empty list"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: (Ord t, Num t) => t -> a -> [a]
replicate' n x | n <= 0 = []
replicate' n x = x : (replicate' (n - 1) x)

take' n _ | n <= 0 = []
take' n [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: Eq t => t -> [t] -> Bool
elem' a [] = False
elem' a (x : _) | a == x = True
elem' a (_ : xs) = elem' a xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let smaller = quickSort [a | a <- xs, a < x]
      bigger = quickSort [a | a <- xs, a >= x]
   in smaller ++ [x] ++ bigger