{-# LANGUAGE ViewPatterns #-}

module RealWorldHaskell.CH04.List () where

length' :: [a] -> Int
length' [] = 0
length' (x : xs) = length xs + 1

null' :: [a] -> Bool
null' [] = True
null' _ = False

head' :: [p] -> p
head' [] = error "empty list"
head' (x : xs) = x

tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (x : xs) = xs

last' :: [p] -> p
last' [] = error "empty list"
last' (x : []) = x
last' (x : xs) = last' xs

init' :: [a] -> [a]
init' [] = error "empty list"
init' (x : []) = []
init' (x : xs) = x : init' xs

append :: [a] -> [a] -> [a]
append xs ys = foldRight' (:) ys xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = append x (concat' xs)

reverse' :: [a] -> [a]
reverse' = reverseS []
  where
    reverseS xs [] = xs
    reverseS xs (y : ys) = reverseS (y : xs) ys

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x : xs) = x || or' xs

all' :: (t -> Bool) -> [t] -> Bool
all' f [] = True
all' f (x : xs) = f x && all' f xs

any' :: (t -> Bool) -> [t] -> Bool
any' f [] = False
any' f (x : xs) = f x || any' f xs

take' :: (Eq t, Num t) => t -> [a] -> [a]
take' n [] = []
take' 0 xs = []
take' n (x : xs) = x : take' (n - 1) xs

drop' :: (Eq t, Num t) => t -> [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (x : xs) = drop' (n - 1) xs

splitAt' :: (Eq a1, Num a1) => a1 -> [a2] -> ([a2], [a2])
splitAt' n [] = ([], [])
splitAt' 0 xs = ([], xs)
splitAt' n (x : xs) =
  let (l, r) = splitAt' (n - 1) xs
   in (x : l, r)

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

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' f [] = ([], [])
break' f (x : xs)
  | f x = ([], x : xs)
  | otherwise =
    let (l, r) = break' f xs
     in (x : l, r)

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' f = break' (not . f)

elem' :: Eq t => t -> [t] -> Bool
elem' x [] = False
elem' x (y : ys) = x == y || elem' x ys

notElem' :: Eq t => t -> [t] -> Bool
notElem' x = not . elem' x

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x : xs) = if f x then x : filter' f xs else filter' f xs

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] ys = True
isPrefixOf' xs [] = False
isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' xs ys = isPrefixOf' (reverse' xs) (reverse' ys)

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' [] ys = True
isInfixOf' xs [] = False
isInfixOf' xs (y : ys) = isPrefixOf' xs (y : ys) || isInfixOf' xs ys

zip' :: [a] -> [b] -> [(a, b)]
zip' xs [] = []
zip' [] ys = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' f xs [] = []
zipWith' f [] ys = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

lines' :: [Char] -> [[Char]]
lines' [] = []
lines' (break' (== '\n') -> (l, '\n' : r)) = l : lines' r
lines' xs = [xs]

unlines' :: [[Char]] -> [Char]
unlines' [] = []
unlines' (x : xs) = x ++ "\n" ++ unlines' xs

isSpace' :: Char -> Bool
isSpace' x = elem' x [' ', '\t', '\n', '\r', '\f', '\v']

words' :: [Char] -> [[Char]]
words' [] = []
words' (break' isSpace' -> (l, dropWhile' isSpace' -> r)) = l : words' r

unwords' :: [[Char]] -> [Char]
unwords' [] = []
unwords' (x : []) = x
unwords' (x : xs) = x ++ " " ++ unwords' xs

fold' :: (b -> a -> b) -> b -> [a] -> b
fold' step i [] = i
fold' step i (x : xs) = fold' step (step i x) xs

flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip' f x y = f y x

foldRight' :: (a -> b -> b) -> b -> [a] -> b
foldRight' step i xs = fold' (flip' step) i (reverse' xs)

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f xs = foldl group [] xs
  where
    group [] x = [[x]]
    group ((y : ys) : xss) x =
      if f x y
        then (y : ys ++ [x]) : xss
        else (y : ys) : group xss x

-- use fold
takeWhile'' :: (a -> Bool) -> [a] -> ([a], [a])
takeWhile'' f xs =
  let (l, r) = foldl step ([], []) xs
   in (reverse' l, reverse' r)
  where
    step (l, []) x
      | f x = ((x : l), [])
    step (l, r) x = (l, x : r)