module Zippers () where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' Empty Empty)
            (Node 'T' Empty Empty)
        )
        ( Node
            'Y'
            (Node 'S' Empty Empty)
            (Node 'A' Empty Empty)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' Empty Empty)
            (Node 'R' Empty Empty)
        )
        ( Node
            'A'
            (Node 'A' Empty Empty)
            (Node 'C' Empty Empty)
        )
    )

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, xs) = (l, LeftCrumb x r : xs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, xs) = (r, RightCrumb x l : xs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r : xs) = (Node x t r, xs)
goUp (t, RightCrumb x l : xs) = (Node x l t, xs)