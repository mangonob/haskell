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

twice :: (t -> t) -> t -> t
twice f x = f (f x)

zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

f :: Ord a => [a] -> [a]
f [] = []
f (x : y) = let l = f (filter (<= x) y); g = f (filter (> x) y) in l ++ [x] ++ g

flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip' f = g where g y x = f x y

map' :: (t -> a) -> [t] -> [a]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n : collatz (div n 2)
  | odd n = n : collatz (n * 3 + 1)

sum2 :: (Num a) => [a] -> a
sum2 = foldl (+) 0

elem2 :: (Foldable t, Eq a) => a -> t a -> Bool
elem2 x = foldl (\curr e -> if e == x then True else curr) False

flat :: [[a]] -> [a]
flat [] = []
flat (x : xs) = x ++ flat xs

flatMap :: (a1 -> [a2]) -> [a1] -> [a2]
flatMap f xs = flat (map f xs)

minusAll :: [Integer] -> [Integer]
minusAll = map (negate . abs)