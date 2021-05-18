module RealWorldHaskell.CH04.Plus () where

plus :: Num a => a -> a -> a
a `plus` b = a + b

data a `Pair` b = a `Pair` b deriving (Show)

foo :: Pair Integer Integer
foo = 1 `Pair` 2

bar :: Pair Bool [Char]
bar = True `Pair` "quux"