module Algo.Graph where

import Algo.Data.Graph (Graph (..), containEdge, fromList)
import Data.List (foldl1')

pow :: Int -> Graph v -> Graph v
pow n (Graph vs m) = Graph vs (fmap signum $ foldl1' (*) $ replicate n m)

quad :: Graph v -> Graph v
quad = pow 2