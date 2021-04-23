module Main where

import Data.Char

main :: IO ()
main = putStrLn $ show 42

intersperse' :: a -> [a] -> [a]
intersperse' a [] = []
intersperse' a (x : []) = x : []
intersperse' a (x : xs) = x : a : intersperse' a xs

transpose' :: [[a]] -> [[a]]
transpose' xs = foldr combine [] xs
  where
    combine [] y = y
    combine (x : xs) [] = [x] : combine xs []
    combine (x : xs) (y : ys) = (x : y) : combine xs ys

concat' :: [a] -> [a] -> [a]
concat' [] y = y
concat' (x : xs) y = x : concat' xs y

concatMap' :: (t -> [a]) -> [t] -> [a]
concatMap' _ [] = []
concatMap' f (x : xs) = f x ++ concatMap' f xs

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x : xs) = x || or' xs

all' :: (a -> Bool) -> [a] -> Bool
all' f xs = and' $ map f xs

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = or' $ map f xs

iterate'' :: (t -> t) -> t -> [t]
iterate'' f x = x : iterate'' f (f x)

splitAt' :: (Ord a1, Num a1) => a1 -> [a2] -> ([a2], [a2])
splitAt' n [] = ([], [])
splitAt' n (x : xs)
  | n > 0 = let (l, r) = splitAt' (n - 1) xs in (x : l, r)
  | otherwise = ([], x : xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x : xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x : xs)
  | f x = dropWhile' f xs
  | otherwise = x : xs

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' f (x : xs)
  | f x = let (l, r) = span' f xs in (x : l, r)
  | otherwise = ([], x : xs)

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' _ [] = ([], [])
break' f (x : xs)
  | f x = ([], x : xs)
  | otherwise = let (l, r) = break' f xs in (x : l, r)

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (x : []) = [[x]]
group' (x : xs) =
  let (l, r) = span' (== x) (x : xs)
   in l : group' r

inits' :: [a] -> [[a]]
inits' xs = [] : t xs
  where
    t [] = []
    t (x : xs) = [[x]] ++ (map (\s -> x : s) $ t xs)

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (x : xs) = [(x : xs)] ++ (tails' xs)

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x : xs) (y : ys)
  | x == y = isPrefixOf' xs ys
  | otherwise = False

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' x y = isPrefixOf' (reverse x) (reverse y)

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' x y = or' $ map (isPrefixOf' x) (tails' y)

elem' :: Eq t => t -> [t] -> Bool
elem' x [] = False
elem' x (y : xs)
  | x == y = True
  | otherwise = elem' x xs

noElem' :: Eq t => t -> [t] -> Bool
noElem' x xs = not $ elem' x xs

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' f (x : xs)
  | f x = (x : l, r)
  | otherwise = (l, x : r)
  where
    (l, r) = partition' f xs

find' :: (a -> Bool) -> [a] -> Maybe a
find' _ [] = Nothing
find' f (x : xs)
  | f x = Just x
  | otherwise = find' f xs

elemIndex' :: (Eq t) => t -> [t] -> Maybe Int
elemIndex' _ [] = Nothing
elemIndex' x (y : xs)
  | x == y = Just 0
  | otherwise = Just (+ 1) <*> elemIndex' x xs

elemIndices' :: (Eq a) => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (y : xs)
  | x == y = 0 : other
  | otherwise = other
  where
    other = (+ 1) <$> elemIndices' x xs

findIndex' :: Num a => (t -> Bool) -> [t] -> Maybe a
findIndex' _ [] = Nothing
findIndex' f (x : xs)
  | f x = Just 0
  | otherwise = Just (+ 1) <*> findIndex' f xs

findIndices' :: Num a => (t -> Bool) -> [t] -> [a]
findIndices' _ [] = []
findIndices' f (x : xs)
  | f x = 0 : other
  | otherwise = other
  where
    other = (+ 1) <$> findIndices' f xs

lines' :: [Char] -> [[Char]]
lines' [] = []
lines' xs = case break' (== '\n') xs of
  (l, []) -> [l]
  (l, (_ : r)) -> l : lines' r

unlines' :: [[Char]] -> [Char]
unlines' [] = []
unlines' (x : xs) = x ++ '\n' : unlines' xs

words' :: [Char] -> [[Char]]
words' [] = []
words' (' ' : xs) = words' xs
words' xs =
  let (l, r) = break' (== ' ') xs
   in l : words' r

unwords' :: [[Char]] -> [Char]
unwords' [] = []
unwords' (x : []) = x
unwords' (x : xs) = x ++ ' ' : unwords' xs

delete' :: Eq t => t -> [t] -> [t]
delete' _ [] = []
delete' x (y : ys)
  | x == y = ys
  | otherwise = y : delete' x ys

subtraction' :: Eq t => [t] -> [t] -> [t]
subtraction' xs [] = xs
subtraction' xs (y : ys) = subtraction' (delete' y xs) ys

union' :: Eq a => [a] -> [a] -> [a]
union' [] ys = ys
union' xs [] = xs
union' xs (y : ys)
  | elem' y xs = union' xs ys
  | otherwise = union' xs ys ++ [y]

intersection' :: Eq a => [a] -> [a] -> [a]
intersection' _ [] = []
intersection' [] _ = []
intersection' xt@(x : xs) yt@(y : ys)
  | x == y = x : intersection' xs ys
  | otherwise = union' (intersection' xs yt) (intersection' ys xt)

insert' :: Ord t => t -> [t] -> [t]
insert' x [] = [x]
insert' x (y : ys)
  | y >= x = x : y : ys
  | otherwise = y : insert' x ys
