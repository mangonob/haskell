{-# LANGUAGE ViewPatterns #-}

import Control.Monad.State

main :: IO ()
main = interact id

newtype Stack a = Stack {toList :: [a]} deriving (Show)

push :: a -> State (Stack a) ()
push x = state $ \(Stack xs) -> ((), Stack (x : xs))
