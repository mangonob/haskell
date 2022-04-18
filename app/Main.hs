module Main where

import Control.Applicative
import Control.Exception (catch)
import Control.Monad (forever, when)
import Control.Monad.State
import Control.Monad.Writer (MonadWriter (tell, writer), Writer, runWriter)
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.DList (DList, fromList, toList)
import Data.Function (on)
import Data.Monoid
import qualified Data.String as B
import GHC.Base (Applicative)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO.Error (ioeGetFileName)
import System.Random

main :: IO ()
main = interact id

sayMe :: (Integral a) => a -> [Char]
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe _ = "<Unknow>"

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort [y | y <- xs, y <= x] ++ x : quickSort [y | y <- xs, y > x]

-- Heron's formula
triangleArea :: Floating a => a -> a -> a -> a
triangleArea a b c =
  let p = (a + b + c) / 2
   in sqrt $ p * (p - a) * (p - b) * (p - c)

reverseWords :: String -> String
reverseWords = unwords . reverse . words

reverseTell :: IO ()
reverseTell = getLine >>= putStrLn . reverseWords

echo :: IO a
echo = forever $ getLine >>= putStrLn

longLineTell :: IO b
longLineTell = forever $ getLine >>= \s -> when (length s >= 10) $ putStrLn s

echoArgs :: IO ()
echoArgs = getArgs >>= mapM_ putStrLn

magic :: (Eq a, Num a) => a -> Bool
magic 42 = True
magic _ = error "Magic number must be 42"

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl evalRPN [] . words
  where
    evalRPN (x : y : xs) "+" = (y + x) : xs
    evalRPN (x : y : xs) "-" = (y - x) : xs
    evalRPN (x : y : xs) "*" = (y * x) : xs
    evalRPN xs n = read n : xs

-- Question: London path
--
-- A ---50---(A1)--- 5---(A2)---40---(A3)---10---(A4)
--            |           |           |           |
--            30          20          25          0
--            |           |           |           |
-- B ---10---(B1)---90---(B2)--- 2---(B3)--- 8---(B4)
-- :run solveLondonPathMain 50 10 30 5 90 20 40 2 25 10 8 0

data Path = Path Int Int Int deriving (Eq, Show)

data Choice = A | B | C deriving (Eq, Show)

solveLondonPathMain :: IO ()
solveLondonPathMain = do
  args <- getArgs
  let nums = map read args :: [Int]
  let (choices, cost) = solveLondonPath nums
  putStrLn $ "Pathes: " ++ show (reverse choices)
  putStrLn $ "Cost: " ++ show cost

solveLondonPath :: [Int] -> ([Choice], Int)
solveLondonPath = _solveLondonPath ([], 0) ([], 0) . _pathes

_pathes :: [Int] -> [Path]
_pathes [] = []
_pathes (a : b : c : xs) = Path a b c : _pathes xs
_pathes (a : b : xs) = Path a b 0 : _pathes xs
_pathes _ = error "error pathes data"

_solveLondonPath :: ([Choice], Int) -> ([Choice], Int) -> [Path] -> ([Choice], Int)
_solveLondonPath (as, acost) (bs, bcost) []
  | acost <= bcost = (as, acost)
  | otherwise = (bs, bcost)
_solveLondonPath (as, acost) (bs, bcost) (Path a b c : ps) =
  let ashort = if a <= b + c then (A : as, acost + a) else (C : B : bs, bcost + b + c)
      bshort = if b <= a + c then (B : bs, bcost + b) else (C : A : as, acost + a + c)
   in _solveLondonPath ashort bshort ps

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (compare `on` length) x y `mappend` (x `compare` y)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

testTree :: Tree Integer
testTree =
  Node
    5
    ( Node
        3
        (Node 1 Empty Empty)
        (Node 6 Empty Empty)
    )
    ( Node
        9
        (Node 8 Empty Empty)
        (Node 10 Empty Empty)
    )

firstIndex :: (Num p, Eq t) => t -> [t] -> p
firstIndex _ [] = -1
firstIndex x (y : xs)
  | x == y = 0
  | otherwise = 1 + firstIndex x xs

onlyUpper :: [Char] -> [Char]
onlyUpper xs = [x | x <- xs, x `elem` ['A' .. 'Z']]

collatz :: Integer -> State [Integer] Integer
collatz 1 = return 1
collatz x
  | even x = record >> collatz (x `div` 2)
  | otherwise = record >> collatz (x * 3 + 1)
  where
    record :: State [Integer] ()
    record = do
      path <- get
      put (x : path)

data Range = Range {lower :: Int, upper :: Int} deriving (Eq)

instance Show Range where
  show (Range l u) = "(" ++ show l ++ " .. " ++ show u ++ ")"

splitRange :: Range -> [Range]
splitRange (Range a b)
  | b <= a = []
  | b == a + 1 = [Range a b]
  | isPower (b - a) && a `mod` (b - a) == 0 = [Range a b]
  | otherwise =
    let r = (b - 1) `div` slot * slot
     in splitRange (Range a r) ++ splitRange (Range r b)
  where
    isPower x = x .&. (x - 1) == 0

    highestBit 0 = 0
    highestBit x = 1 + highestBit (x `shiftR` 1)

    slot = bit (highestBit (b - a) - 1) :: Int

testSplitRange :: IO ()
testSplitRange =
  let ranges = fmap (\x -> Range x (x + 3000)) [1000 .. 2000]
   in mapM_ print (fmap splitRange ranges)

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft a (l, r)
  | abs ((l + a) - r) > 4 = Nothing
  | otherwise = Just (l + a, r)

landRight :: Birds -> Pole -> Maybe Pole
landRight a (l, r)
  | abs (l - (r + a)) > 4 = Nothing
  | otherwise = Just (l, r + a)

banana :: Pole -> Maybe Pole
banana _ = Nothing

optionalChain :: Maybe Integer
optionalChain = do
  x <- Just 3
  y <- Just 4
  return $ x * y

-- Logger monad like Writer
newtype Logger m a = Logger {runLogger :: (a, m)} deriving (Show)

instance Functor (Logger m) where
  fmap f (Logger (a, b)) = Logger (f a, b)

instance (Monoid m) => Applicative (Logger m) where
  pure x = Logger (x, mempty)
  Logger (f, l) <*> Logger (a, l') = Logger (f a, l `mappend` l')

instance (Monoid m) => Monad (Logger m) where
  return x = Logger (x, mempty)
  Logger (a, l) >>= f = let Logger (b, l') = f a in Logger (b, l `mappend` l')

logger :: (Monoid m) => m -> Logger m ()
logger v = Logger ((), v)

gcd' :: (Show t, Integral t) => t -> t -> Logger (DList String) t
gcd' a 0 = do
  logger $ fromList [show a]
  return a
gcd' a b = do
  logger $ fromList ["gcd " ++ show a ++ " " ++ show b ++ " -> "]
  gcd' b (a `mod` b)

readM :: Int -> Int
readM = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)
