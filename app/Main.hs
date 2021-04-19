module Main where

import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  line <- fmap reverse getLine
  putStrLn line

data Action a = Pop a | Top a | Bottom a deriving (Show)

solve :: (Ord a, Num a) => [a] -> [Action a]
solve [] = []
solve xs
  | sum l < sum r = (map (\x -> Bottom x) l) ++ [Pop m] ++ solve (t ++ l)
  | otherwise = (map (\x -> Top x) (reverse r)) ++ [Pop m] ++ solve (t ++ l)
  where
    m = minimum xs
    (l, r) = break (== m) xs
    t = tail r

data Optional a = Nil | Some a deriving (Show)

instance Functor Optional where
  fmap _ Nil = Nil
  fmap f (Some x) = Some $ f x

instance Applicative Optional where
  pure x = Some x
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Some f <*> Some x = Some $ f x

instance Monad Optional where
  return x = Some x
  Nil >>= _ = Nil
  Some x >>= f = f x

landLeft :: (Ord b, Num b) => b -> (b, b) -> Maybe (b, b)
landLeft x (left, right)
  | abs (left + x - right) < 4 = Just (left + x, right)
  | otherwise = Nothing

landRight :: (Ord b, Num b) => b -> (b, b) -> Maybe (b, b)
landRight x (left, right)
  | abs (right + x - left) < 4 = Just (left, right + x)
  | otherwise = Nothing

moveNight :: (Int, Int) -> [(Int, Int)]
moveNight (x, y) = do
  (x', y') <-
    [ (x + 1, y + 2),
      (x + 1, y - 2),
      (x - 1, y + 2),
      (x - 1, y - 2),
      (x + 2, y + 1),
      (x + 2, y - 1),
      (x - 2, y + 1),
      (x - 2, y - 1)
      ]
  guard (elem x' [1 .. 8] && elem y' [1 .. 8])
  return (x', y')

-- K步可以移动到的点
moveK :: Int -> (Int, Int) -> [(Int, Int)]
moveK k (x, y) = nub $ foldl (>>=) (return (x, y)) $ replicate k moveNight

-- K步可以移动到的点
moveInK :: Int -> (Int, Int) -> [(Int, Int)]
moveInK k (x, y) = nub $ concat $ moveK <$> [1 .. k] <*> pure (x, y)

-- 到达棋盘上所有点的最少步数的最大值
minStep :: (Int, Int) -> Int
minStep (x, y) = succ $ length $ takeWhile (/= 64) $ fmap length $ moveInK <$> [1 ..] <*> pure (x, y)

allStep :: [((Int, Int), Int)]
allStep = do
  x <- [1 .. 8]
  y <- [1 .. 8]
  return ((x, y), minStep (x, y))