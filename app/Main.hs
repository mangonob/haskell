{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import System.Random
import Text.Read
import Utils

main :: IO ()
main = interact id

newtype Stack a = Stack {toList :: [a]} deriving (Show)

push x (Stack xs) = ((), Stack (x : xs))

pop (Stack []) = (Nothing, Stack [])
pop (Stack (x : xs)) = (Just x, Stack xs)

pushS :: a -> State (Stack a) ()
pushS x = state $ push x

popS :: State (Stack a) (Maybe a)
popS = state pop

randomSt :: (Random a, RandomGen g) => State g a
randomSt = state $ random

threeCoin :: RandomGen g => State g (Bool, Bool, Bool)
threeCoin = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

errorMonadExample :: Either String Integer
errorMonadExample = do
  Right 3
  Right 3

liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2' f m1 m2 = do
  x <- m1
  y <- m2
  return $ f x y

join' :: Monad m => m (m a) -> m a
join' m = do
  x <- m
  x

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' f [] = return []
filterM' f (x : xs) = do
  p <- f x
  ys <- filterM' f xs
  if p
    then return (x : ys)
    else return ys

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping: " ++ show x]
    return True
  | otherwise = do
    tell [show x ++ "5 is too large, throwing it away"]
    return False

powerset :: [a] -> [[a]]
powerset xs = filterM (return [True, False]) xs

foldM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM' f b [] = return b
foldM' f b (x : xs) = do
  c <- f b x
  foldM' f c xs

binSmall :: Int -> Int -> Maybe Int
binSmall acc x
  | x > 9 = Nothing
  | otherwise = Just $ acc + x

solveRPN :: String -> Maybe Double
solveRPN st = do
  [a] <- foldM foldRPN [] (words st)
  return a

foldRPN :: [Double] -> String -> Maybe [Double]
foldRPN (x : y : ys) "*" = return ((y * x) : ys)
foldRPN (x : y : ys) "+" = return ((y + x) : ys)
foldRPN (x : y : ys) "-" = return ((y - x) : ys)
foldRPN (x : y : ys) "/" = return ((y / x) : ys)
foldRPN xs numberString = liftM (: xs) (readMaybe numberString)

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (x, y) =
  filter inRange $
    map
      (add (x, y))
      [ (1, 2),
        (1, -2),
        (-1, 2),
        (-1, -2),
        (2, 1),
        (2, -1),
        (-2, 1),
        (-2, -1)
      ]
  where
    inRange (a, b) = elem a [1 .. 8] && elem b [1 .. 8]
    add (x, y) (z, w) = (x + z, y + w)

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldl (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = elem end $ inMany x start