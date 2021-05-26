module RealWorldHaskell.CH14.Chore where

import Control.Monad
import Control.Monad.State
  ( MonadState (state),
    State,
    guard,
    runState,
  )
import System.Random (Random (randomR), RandomGen)

multipleTo :: Int -> [(Int, Int)]
multipleTo n = do
  a <- [1 .. n]
  b <- [a .. n]
  guard $ (a * b) == n
  return (a, b)

multipleTo' :: Int -> [(Int, Int)]
multipleTo' n =
  [1 .. n] >>= \a ->
    [a .. n] >>= \b ->
      guard (a * b == n)
        >> return (a, b)

genR :: RandomGen g => (Integer, Integer) -> State g Integer
genR (a, b) = state $ \g -> let (n, ng) = randomR (a, b) g in (n, ng)

gen1_100 :: RandomGen g => State g Integer
gen1_100 = genR (1, 100)

genN :: RandomGen g => Int -> g -> [Integer]
genN n g = fst $ (runState $ sequence $ replicate n gen1_100) g