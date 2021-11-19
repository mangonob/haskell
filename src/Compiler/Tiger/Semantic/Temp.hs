module Compiler.Tiger.Semantic.Temp where

import Compiler.Tiger.Symbol
import Control.Monad.State.Strict

type Label = Sym

data TempS = TempS {temps :: Int, labs :: Int, num :: Int} deriving (Show, Eq)

type S a = State TempS a

emptyS :: TempS
emptyS = TempS 0 0 0

createS :: S ()
createS = put emptyS

newTemp :: S Int
newTemp = do
  s@TempS {temps = n} <- get
  put s {temps = n + 1}
  return n

newLabel :: S Label
newLabel = do
  s@TempS {labs = l} <- get
  put s {labs = l + 1}
  return $ "L" ++ show l

newNum :: S Int
newNum = do
  s@TempS {num = n} <- get
  put s {num = n + 1}
  return n

_test :: S Int
_test = do
  newTemp
  newTemp
  newTemp
  newTemp