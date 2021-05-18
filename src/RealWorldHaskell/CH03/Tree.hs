module RealWorldHaskell.CH03.Tree () where

data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

simpleTree :: Tree [Char]
simpleTree = Node "parent" (Node "left child" Empty Empty) (Node "right child" Empty Empty)

height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = max (height l) (height r) + 1