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

red :: RBTree a -> RBTree a
red Empty = Empty
red (Node _ x l r) = Node Red x l r

black :: RBTree a -> RBTree a
black Empty = Empty
black (Node _ x l r) = Node Black x l r

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
_insert x Empty = red $ singleton x
_insert x t@(Node c y l r)
  | x == y = t
  | x < y = insertFixup $ Node c y (_insert x l) r
  | x > y = insertFixup $ Node c y l (_insert x r)
  | otherwise = undefined

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
rotateR _ = error "Can't right rotate a node without left child"

rotateL :: RBTree a -> RBTree a
rotateL (Node c1 x l1 (Node c2 y l2 r2)) = Node c2 y (Node c1 x l1 l2) r2
rotateL _ = error "Can't left rotate a node without right child"

blackHeight :: Show a => RBTree a -> Int
blackHeight Empty = 0
blackHeight t@(Node Red _ l r) =
  if blackHeight l == blackHeight r
    then blackHeight l
    else error "bad red black tree"
blackHeight t@(Node Black _ l r) =
  if blackHeight l == blackHeight r
    then blackHeight l + 1
    else error "bad red black tree"

fromList :: Ord a => [a] -> RBTree a
fromList = foldl (flip insert) Empty

treeMin :: RBTree a -> a
treeMin (Node _ x Empty _) = x
treeMin (Node _ x left _) = treeMin left

treeMax :: RBTree a -> a
treeMax (Node _ x _ Empty) = x
treeMax (Node _ x _ right) = treeMax right

remove :: Ord a => a -> RBTree a -> RBTree a
remove x t = fst $ _remove x t

_remove :: Ord a => a -> RBTree a -> (RBTree a, Bool)
_remove x Empty = (Empty, True)
_remove x (Node c y l r)
  | x > y = let (newR, xr) = _remove x r in removeFixup (Node c y l newR) (True, xr)
  | x < y = let (newL, xl) = _remove x l in removeFixup (Node c y newL r) (xl, True)
  -- x == y
  | l == Empty && r == Empty = (Empty, c == Red)
  | l /= Empty =
    let prev = treeMax l
        (newL, xl) = _removeMax l
     in removeFixup (Node c prev newL r) (xl, True)
  | r /= Empty = removeFixup (black r) (True, c == Red || isRed r)

_removeMax :: RBTree a -> (RBTree a, Bool)
_removeMax (Node Red _ left Empty) = (black left, True)
_removeMax (Node Black _ left Empty) = (black left, isRed left)
_removeMax (Node c x left right) =
  let (r, rx) = _removeMax right
   in removeFixup (Node c x left r) (True, rx)

tint :: Color -> RBTree a -> RBTree a
tint c = if c == Red then red else black

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
  | isRed $ left r = (tint c $ rotateL $ Node Black x l (rotateR r), True)
  | otherwise = (rotateL $ Node Red x l r, c == Red)
removeFixup p@(Node c x l r) (True, False)
  | isRed r = (Node c x l (black r), True)
  | isRed l =
    let (Node cy y yl yr) = rotateR $ Node Red x (black l) r
        (newR, xr) = removeFixup yr (True, False)
     in removeFixup (Node cy y yl newR) (True, xr)
  -- left node is black
  | isRed $ right l = (tint c $ rotateR $ Node Black x (rotateL l) r, True)
  | otherwise = (rotateR $ Node Red x l r, c == Red)
