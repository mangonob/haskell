{-# LANGUAGE ViewPatterns #-}

module Algo.Data.Matrix
  ( Matrix (..),
    transpose,
    singleton,
    empty,
    insert,
    insertAt,
    insertCol,
    insertColAt,
    insertRow,
    insertRowAt,
    row,
    col,
    elemAt,
    update,
  )
where

import qualified Data.List as L

newtype Matrix a = Matrix {fromList :: [[a]]} deriving (Eq, Show)

singleton :: a -> Matrix a
singleton a = Matrix []

empty :: Matrix a
empty = Matrix []

instance Num a => Num [a] where
  [] + [] = []
  (x : xs) + (y : ys) = x + y : xs + ys
  _ + _ = error "vectors have different size"

  [] - [] = []
  (x : xs) - (y : ys) = x - y : xs - ys
  _ - _ = error "vectors have different size"

  [] * [] = []
  (x : xs) * (y : ys) = x * y : xs * ys
  _ * _ = error "vectors have different size"

  negate [] = []
  negate (x : xs) = - x : negate xs

  abs [] = []
  abs (x : xs) = abs x : abs xs

  signum [] = []
  signum (x : xs) = signum x : signum xs

  fromInteger x = [fromInteger x]

instance Functor Matrix where
  fmap f (Matrix xs) = Matrix (map (map f) xs)

(.*) :: Num a => [a] -> [a] -> a
a .* b = sum $ a * b

instance Num a => Num (Matrix a) where
  Matrix xs + Matrix ys = Matrix (xs + ys)
  Matrix xs - Matrix ys = Matrix (xs - ys)
  Matrix xs * y =
    let Matrix ys = transpose y
     in Matrix $ map (\x -> map (x .*) ys) xs

  negate (Matrix xs) = Matrix (- xs)

  abs _ = error "not implemented"

  signum _ = error "not implemented"

  fromInteger x = Matrix (fromInteger x)

transpose :: Matrix a -> Matrix a
transpose (Matrix xs) = Matrix (L.transpose xs)

width :: Matrix a -> Int
width (Matrix []) = 0
width (Matrix (x : _)) = length x

height :: Matrix a -> Int
height (Matrix xs) = length xs

insertRowAt :: Int -> a -> Matrix a -> Matrix a
insertRowAt i a m@(Matrix xs) =
  let (l, r) = splitAt i xs
   in Matrix $ l ++ replicate (width m) a : r

insertRow :: a -> Matrix a -> Matrix a
insertRow = insertRowAt 0

insertColAt :: Int -> a -> Matrix a -> Matrix a
insertColAt i a (Matrix xs) =
  Matrix $ map insertE xs
  where
    insertE xs =
      let (l, r) = splitAt i xs
       in l ++ a : r

insertCol :: a -> Matrix a -> Matrix a
insertCol = insertColAt 0

insertAt :: Int -> a -> Matrix a -> Matrix a
insertAt i a = insertColAt i a . insertRowAt i a

insert :: a -> Matrix a -> Matrix a
insert = insertAt 0

row :: Int -> Matrix a -> [a]
row i (Matrix xs) = xs !! i

col :: Int -> Matrix b -> [b]
col i (Matrix xs) = map (!! i) xs

elemAt :: Int -> Int -> Matrix a -> a
elemAt i j (Matrix xs) = xs !! i !! j

update :: Int -> Int -> a -> Matrix a -> Matrix a
update i j v (Matrix xs) = Matrix $ updateL i (updateL j (const v)) xs
  where
    updateL i f (splitAt i -> (xs, [])) = xs
    updateL i f (splitAt i -> (xs, (y : ys))) = xs ++ f y : ys