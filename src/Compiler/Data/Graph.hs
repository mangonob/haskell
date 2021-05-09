module Compiler.Data.Graph () where

import qualified Data.Map as M
import qualified Data.Set as S

type Graph v e = M.Map v (S.Set (Edge v e))

type Edge v e = (e, v)

vectexes :: Graph v e -> [v]
vectexes g = M.keys g