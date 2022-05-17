module Algo.DynamicProgramming.CutRod where

import Control.Monad.State
import Data.Foldable (maximumBy)
import Data.Function (on)

cutRod :: (Ord a, Num a) => Int -> State [a] [Int]
cutRod n = do
  schemes <- mapM (\x -> fmap (x :) (cutRod (n - x))) [1 .. n - 1]
  prices <- get
  return $ maximumBy (compare `on` (sum . fmap (prices !!))) ([n] : schemes)

cutRodExamples :: ([[Int]], [Integer])
cutRodExamples = runState (mapM cutRod [1 .. 10]) [0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30]

cutRodBigExamples :: ([[Int]], [Integer])
cutRodBigExamples = runState (mapM cutRod [1 .. 100]) [1 .. 10000]