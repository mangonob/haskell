module RealWorldHaskell.CH14.Logger where

-- 日志收集
newtype Logger a = Logger {runLogger :: (a, [String])}
  deriving (Show)

-- 记录日志
record :: String -> Logger ()
record xs = Logger ((), [xs])

instance Functor Logger where
  fmap f (Logger (a, logs)) = Logger (f a, logs)

instance Applicative Logger where
  pure x = Logger (x, [])
  Logger (f, fLogs) <*> Logger (a, aLogs) = Logger (f a, fLogs ++ aLogs)

instance Monad Logger where
  return = pure

  Logger (x, oldLogs) >>= f =
    let Logger (y, newLogs) = f x
     in Logger (y, oldLogs ++ newLogs)

gcdWithLogger :: Integer -> Integer -> Logger Integer
gcdWithLogger a 0 = return a
gcdWithLogger a b = do
  record $ "calculated gcd of " ++ show a ++ ", " ++ show b
  gcdWithLogger b (mod a b)