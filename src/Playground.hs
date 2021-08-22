module Playground where

data LinkedList a = Node (LinkedList a) a | Null deriving (Show)