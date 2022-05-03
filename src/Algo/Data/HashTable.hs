module Algo.Data.HashTable where

class Hashable h where
  hash :: h -> Int

instance Hashable Int where
  hash = id