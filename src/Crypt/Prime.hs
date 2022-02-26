module Crypt.Prime (isPrime, modPow) where

depart :: (Num a, Integral b) => b -> (a, b)
depart n
  | n <= 0 = error "Depart a nopositive number"
  | even n = let (s, t) = depart $ div n 2 in (s + 1, t)
  | otherwise = (0, n)

isPrime :: Integer -> Bool
isPrime x
  | x < 30 = x `elem` primer
  | otherwise = and $ millerRobin <$> primer <*> pure x

millerRobin :: Integral a => a -> a -> Bool
millerRobin p x =
  let (s, t) = depart (x - 1)
      testor = take (s + 1) $ iterate (modPow x 2) $ modPow x t p
   in let det = and $ map (\n -> n == 1 || n == x - 1) $ filter (\n -> modPow x 2 n == 1) $ init testor
       in det && (mod (last testor) x == 1)

mod' :: Integral a => a -> a -> a
mod' a b = mod b a

modPow :: Integral a => a -> a -> a -> a
modPow n 0 x = 1
modPow n 1 x = mod x n
modPow n p x
  | x > n = modPow n p (mod' n x)
  | even p = mod' n $ modPow n (div p 2) (x * x)
  | otherwise = mod' n $ x * modPow n (div p 2) (x * x)

primer :: [Integer]
primer = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]