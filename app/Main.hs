module Main where

main :: IO ()
main = do
  nStr <- getLine
  let n = read nStr :: Int
  cal n

cal :: Int -> IO ()
cal 0 = return ()
cal n = do
  nums <- getLine
  let (fromS : toS : _) = words nums
  let from = read fromS :: Integer
  let to = read toS :: Integer
  putAllPrime [from .. to]
  putStrLn ""
  cal $ n - 1

putAllPrime :: [Integer] -> IO ()
putAllPrime [] = return ()
putAllPrime (x : xs) = do
  if isPrime x
    then putStrLn $ show x
    else return ()
  putAllPrime xs

depart :: (Num a, Integral b) => b -> (a, b)
depart n
  | n <= 0 = error "Depart a nopositive number"
  | mod n 2 == 0 = let (s, t) = depart $ div n 2 in (s + 1, t)
  | otherwise = (0, n)

isPrime :: Integer -> Bool
isPrime x
  | x < 30 = elem x primer
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
  | mod p 2 == 0 = mod' n $ modPow n (div p 2) (x * x)
  | otherwise = mod' n $ x * modPow n (div p 2) (x * x)

primer :: [Integer]
primer = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]