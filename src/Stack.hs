module Stack where

import Control.Monad.State

type Stack a = [a]

pop :: State (Stack a) a
pop = state $ \(x : xs) -> (x, xs)

push :: a -> State (Stack a) ()
push a = state $ \xs -> ((), a : xs)

stackManip :: State (Stack Int) Int
stackManip = do
  push 3
  a <- pop
  pop