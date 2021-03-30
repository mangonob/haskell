module Main where

import Data.Map (Map, fromList, lookup)
import Prelude hiding (lookup)

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

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Eq, Show, Read)

data Vector a = Vector a a a deriving (Show)

add :: Num a => Vector a -> Vector a -> Vector a
add (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

mult :: Num a => Vector a -> Vector a -> Vector a
mult (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 * x2) (y1 * y2) (z1 * z2)

salarMult :: Num a => Vector a -> Vector a -> a
salarMult (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

type PhoneBook = [(String, String)]

phoneBook :: PhoneBook
phoneBook =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

inPhoneBook :: String -> String -> PhoneBook -> Bool
inPhoneBook name phone book = (name, phone) `elem` book

type AssociatedList k v = [(k, v)]

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup n map =
  case lookup n map of
    Nothing -> Left $ "Locker number " ++ show n ++ " doesn't exist!"
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show n ++ " is aready taken!"

lockers :: LockerMap
lockers =
  fromList
    [ (100, (Taken, "ZD39I")),
      (101, (Free, "JAH3I")),
      (103, (Free, "IQSA9")),
      (105, (Free, "QOTSA")),
      (109, (Taken, "893JJ")),
      (110, (Taken, "99292"))
    ]