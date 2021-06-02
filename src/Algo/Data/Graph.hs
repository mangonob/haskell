module Algo.Data.Graph where

import Algo.Data.Matrix (Matrix, col, elemAt, row)
import qualified Algo.Data.Matrix as M
import Data.List (elemIndex, find, findIndex, foldl')
import Utils (just, success, (??))
import Prelude hiding (concat)

-- | Graph e v
-- * e: Edge
-- * v: Vectex
data Graph v = Graph [v] (Matrix Int) deriving (Show)

instance Functor Graph where
  fmap f (Graph vs es) = Graph (map f vs) es

empty :: Graph v
empty = Graph [] M.empty

containEdge_ :: Eq a => a -> a -> Graph a -> Maybe Bool
containEdge_ v1 v2 (Graph vs m) = do
  i <- elemIndex v1 vs
  j <- elemIndex v2 vs
  return $ elemAt i j m > 0

containEdge :: Eq a => a -> a -> Graph a -> Bool
containEdge v1 v2 g = (just . (?? Just False)) $ containEdge_ v1 v2 g

adj :: Eq v => v -> Graph v -> [v]
adj v (Graph vs m) = case elemIndex v vs of
  Nothing -> []
  Just i -> map snd $ filter ((> 0) . fst) $ zip (row i m) vs

concat :: Eq v => Graph v -> Graph v -> Graph v
concat = undefined

insert :: Eq v => v -> v -> Graph v -> Graph v
insert v1 v2 g = addEdge v1 v2 $ addVertex v2 $ addVertex v1 g

changeAt :: Int -> (a -> a) -> [a] -> [a]
changeAt i f xs = map (\(idx, v) -> if idx == i then f v else v) $ zip [0 ..] xs

addVertex :: Eq v => v -> Graph v -> Graph v
addVertex v g@(Graph vs m) = case elemIndex v vs of
  Nothing -> Graph (v : vs) (M.insert 0 m)
  Just _ -> g

addEdge :: Eq v => v -> v -> Graph v -> Graph v
addEdge v1 v2 (Graph vs m) =
  let Just i = elemIndex v1 vs
      Just j = elemIndex v2 vs
   in Graph vs (M.update i j 1 m)

fromList :: Eq v => [(v, v)] -> Graph v
fromList xs = foldl' (\g (v1, v2) -> insert v1 v2 g) empty xs

toList :: Graph v -> [(v, e, v)]
toList = undefined