{-# LANGUAGE ViewPatterns #-}

data JList a
  = Empty
  | Single a
  | Join (JList a) (JList a)
  deriving (Show)

data JListView a
  = Nil
  | Cons a (JList a)
  deriving (Show)

view :: JList a -> JListView a
view Empty = Nil
view (Single a) = Cons a Empty
view (Join (view -> Cons x xs) y) = Cons x $ Join xs y
view (Join (view -> Nil) y) = view y

length' :: Num p => JList a -> p
length' (view -> Nil) = 0
length' (view -> Cons x xs) = 1 + length' xs