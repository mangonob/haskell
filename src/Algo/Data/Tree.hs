module Algo.Data.Tree where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

singleton :: a -> Tree a
singleton x = Node x Empty Empty

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = singleton x
insert x (Node y l r)
  | x < y = Node y (insert x l) r
  | x > y = Node y l (insert x r)
  | x == y = Node y l r
  | otherwise = undefined

remove :: Ord a => a -> Tree a -> Tree a
remove x Empty = Empty
remove x (Node y l r)
  | x < y = Node y (remove x l) r
  | x > y = Node y l (remove x r)
  | x == y =
    let (l', m) = removeMax l
     in case m of
          Nothing -> r
          Just m -> Node m l' r
  | otherwise = undefined
  where
    removeMax Empty = (Empty, Nothing)
    removeMax (Node y l Empty) = (l, Just y)
    removeMax (Node y l r) =
      let (r', m) = removeMax r
       in (Node y l r', m)

fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insert) Empty

toList :: Tree a -> [a]
toList Empty = []
toList (Node x l r) = toList l ++ x : toList r