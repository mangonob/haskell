module RealWorldHaskell.CH14.Optional where

data Optional a = Nil | Some a deriving (Eq, Show)

instance Functor Optional where
  fmap f Nil = Nil
  fmap f (Some x) = Some (f x)

instance Applicative Optional where
  pure x = Some x
  Some f <*> Some x = Some $ f x

instance Monad Optional where
  return = pure

  Nil >>= f = Nil
  Some x >>= f = f x

instance MonadFail Optional where
  fail err = Nil

optionalChain :: Optional [Char]
optionalChain = do
  x <- Some 3
  y <- Some "4"
  -- fail "some error"
  return $ show x ++ " & " ++ y