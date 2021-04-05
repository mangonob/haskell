module RBTree
  ( RBTree (..),
    Color (..),
    insert,
    singleton,
    red,
    black,
    isRed,
    isBlack,
    blackHeight,
    rotateL,
    rotateR,
    fromList,
  )
where

data Color = Red | Black deriving (Show, Eq)

data RBTree a
  = Empty
  | Node {color :: Color, value :: a, left :: (RBTree a), right :: (RBTree a)}
  deriving (Show)

quotes :: [Char] -> [Char]
quotes x = '"' : x ++ "\""

newtype PrettyRBTree a = PrettyRBTree {getRBTree :: RBTree a}

instance (Show a) => Show (PrettyRBTree a) where
  show (PrettyRBTree {getRBTree = Empty}) = quotes "Empty"
  show (PrettyRBTree {getRBTree = Node c x l r}) =
    "{"
      ++ "color: "
      ++ ( quotes $ case c of
             Red -> "Red"
             Black -> "Black"
         )
      ++ ", value: "
      ++ show x
      ++ ", left: "
      ++ (show $ PrettyRBTree l)
      ++ ", right: "
      ++ (show $ PrettyRBTree r)
      ++ "}"

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

insert :: Ord a => a -> RBTree a -> RBTree a
insert x t = black $ _insert x t

_insert :: Ord a => a -> RBTree a -> RBTree a
_insert x Empty = red $ singleton x
_insert x t@(Node c y l r)
  | x == y = t
  | x < y = insertFixup $ Node c y (_insert x l) r
  | x > y = insertFixup $ Node c y l (_insert x r)

insertFixup :: RBTree a -> RBTree a
insertFixup (Node Black x l r)
  | isRed l && isRed r = Node Red x (black l) (black r)
  | isRed l && isRed (right l) = insertFixup $ Node Black x (rotateL l) r
  | isRed l && isRed (left l) = rotateR $ Node Red x (black l) r
  | isRed r && isRed (left r) = insertFixup $ Node Black x l (rotateR r)
  | isRed r && isRed (right r) = rotateL $ Node Red x l (black r)
insertFixup x = x

rotateR :: RBTree a -> RBTree a
rotateR (Node c1 x (Node c2 y l2 r2) r1) = (Node c2 y l2 (Node c1 x r2 r1))
rotateR _ = error "Can't right rotate a node without left child"

rotateL :: RBTree a -> RBTree a
rotateL Empty = Empty
rotateL (Node c1 x l1 (Node c2 y l2 r2)) = (Node c2 y (Node c1 x l1 l2) r2)
rotateL _ = error "Can't left rotate a node without right child"

blackHeight :: Show a => RBTree a -> Int
blackHeight Empty = 0
blackHeight t@(Node Red _ l r) =
  if blackHeight l == blackHeight r
    then blackHeight l
    else error $ "bad red black tree"
blackHeight t@(Node Black _ l r) =
  if blackHeight l == blackHeight r
    then blackHeight l + 1
    else error $ "bad red black tree"

fromList :: Ord a => [a] -> RBTree a
fromList xs = foldl append Empty xs
  where
    append t x = insert x t