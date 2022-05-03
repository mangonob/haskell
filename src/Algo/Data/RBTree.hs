module Algo.Data.RBTree
  ( RBTree (..),
    Color (..),
    insert,
    singleton,
    fromList,
    remove,
  )
where

data Color = Red | Black deriving (Show, Eq)

data RBTree a
  = Empty
  | Node {color :: Color, value :: a, left :: RBTree a, right :: RBTree a}
  deriving (Show, Eq)

singleton :: a -> RBTree a
singleton x = Node Black x Empty Empty

to :: Color -> RBTree a -> RBTree a
to _ Empty = Empty
to c (Node _ x l r) = Node c x l r

red :: RBTree a -> RBTree a
red = to Red

black :: RBTree a -> RBTree a
black = to Black

isRed :: RBTree a -> Bool
isRed Empty = False
isRed (Node c _ _ _) = c == Red

isBlack :: RBTree a -> Bool
isBlack Empty = True
isBlack (Node c _ _ _) = c == Black

--- Insert
insert :: Ord a => a -> RBTree a -> RBTree a
insert x t = black $ _insert x t

_insert :: Ord a => a -> RBTree a -> RBTree a
_insert x Empty = red (singleton x)
_insert x t@(Node c y l r)
  | x < y = insertFixup $ Node c y (_insert x l) r
  | x > y = insertFixup $ Node c y l (_insert x r)
  | otherwise = t

insertFixup :: RBTree a -> RBTree a
insertFixup (Node Black x l r)
  | isRed l && isRed r = Node Red x (black l) (black r)
  | isRed l && isRed (right l) = insertFixup $ Node Black x (rotateL l) r
  | isRed l && isRed (left l) = rotateR $ Node Red x (black l) r
  | isRed r && isRed (left r) = insertFixup $ Node Black x l (rotateR r)
  | isRed r && isRed (right r) = rotateL $ Node Red x l (black r)
insertFixup x = x

rotateR :: RBTree a -> RBTree a
rotateR (Node c1 x (Node c2 y l2 r2) r1) = Node c2 y l2 (Node c1 x r2 r1)
rotateR _ = error "can't right rotate a node without left child"

rotateL :: RBTree a -> RBTree a
rotateL (Node c1 x l1 (Node c2 y l2 r2)) = Node c2 y (Node c1 x l1 l2) r2
rotateL _ = error "can't left rotate a node without right child"

blackHeight :: RBTree a -> Int
blackHeight Empty = 0
blackHeight t@(Node c _ l r) =
  if blackHeight l == blackHeight r
    then if c == Red then blackHeight l else blackHeight l + 1
    else undefined

fromList :: Ord a => [a] -> RBTree a
fromList = foldl (flip insert) Empty

treeMax :: RBTree a -> a
treeMax (Node _ x _ Empty) = x
treeMax (Node _ x _ right) = treeMax right
treeMax _ = undefined

remove :: Ord a => a -> RBTree a -> RBTree a
remove x t = fst $ _remove x t

_remove :: Ord a => a -> RBTree a -> (RBTree a, Bool)
_remove x Empty = (Empty, True)
_remove x (Node c y l r)
  | x > y = let (newR, xr) = _remove x r in removeFixup (Node c y l newR) (True, xr)
  | x < y = let (newL, xl) = _remove x l in removeFixup (Node c y newL r) (xl, True)
  | x == y && l == Empty && r == Empty = (Empty, c == Red)
  | x == y && l /= Empty =
    let prev = treeMax l
        (newL, xl) = removeMax l
     in removeFixup (Node c prev newL r) (xl, True)
  | x == y && r /= Empty = removeFixup (black r) (True, c == Red || isRed r)
  | otherwise = undefined

removeMax :: RBTree a -> (RBTree a, Bool)
removeMax (Node c _ l Empty) = (l, c == Red)
removeMax (Node c x l r) =
  let (r', rx) = removeMax r
   in removeFixup (Node c x l r') (True, rx)
removeMax _ = undefined

-- | Parameters
--  * RBTree a :
--  * (Bool, Bool): If left and right children is regular(have the right black height).
--  * (RBTree a, Bool): Fixuped Red Black Tree, and if is regular
removeFixup :: RBTree a -> (Bool, Bool) -> (RBTree a, Bool)
removeFixup x (True, True) = (x, True)
removeFixup x (False, False) = (x, False)
removeFixup p@(Node c x l r) (False, True)
  | isRed l = (Node c x (black l) r, True)
  | isRed r =
    let (Node cy y yl yr) = rotateL $ Node Red x l (black r)
        (newL, xl) = removeFixup yl (False, True)
     in removeFixup (Node cy y newL yr) (xl, True)
  -- right node is black
  | isRed (left r) = (to c $ rotateL $ Node Black x l (rotateR r), True)
  | otherwise = (rotateL $ Node Red x l r, c == Red)
removeFixup p@(Node c x l r) (True, False)
  | isRed r = (Node c x l (black r), True)
  | isRed l =
    let (Node cy y yl yr) = rotateR $ Node Red x (black l) r
        (newR, xr) = removeFixup yr (True, False)
     in removeFixup (Node cy y yl newR) (True, xr)
  -- left node is black
  | isRed (right l) = (to c $ rotateR $ Node Black x (rotateL l) r, True)
  | otherwise = (rotateR $ Node Red x l r, c == Red)
removeFixup _ _ = undefined
