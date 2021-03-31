module Tree (Tree (..), singleton, treeInsert, treeElem) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- 向二叉搜索树中插入元素
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x tree@(Node a left right)
  | x == a = tree
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

-- 判断元素是否在二叉搜索树中
treeElem :: Ord a => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

--- $ let nums = [8,6,4,1,7,3,5]
--- $ foldr treeInsert EmptyTree nums
--- Output: Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)