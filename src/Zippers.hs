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

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: (Zipper a) -> (Zipper a)
goLeft (Node x l r, xs) = (l, LeftCrumb x r : xs)

goRight :: (Zipper a) -> (Zipper a)
goRight (Node x l r, xs) = (r, RightCrumb x l : xs)

goUp :: (Zipper a) -> (Zipper a)
goUp (t, LeftCrumb x r : xs) = (Node x t r, xs)
goUp (t, RightCrumb x l : xs) = (Node x l t, xs)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

modify :: (a -> a) -> (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost $ goUp z

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b : bs) = (b : xs, bs)