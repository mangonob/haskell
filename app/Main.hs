module Main where

main :: IO ()
main = do
  line <- fmap reverse getLine
  putStrLn line

data Action a = Pop a | Top a | Bottom a deriving (Show)

solve :: (Ord a, Num a) => [a] -> [Action a]
solve [] = []
solve xs
  | sum l < sum r = (map (\x -> Bottom x) l) ++ [Pop m] ++ solve (t ++ l)
  | otherwise = (map (\x -> Top x) (reverse r)) ++ [Pop m] ++ solve (t ++ l)
  where
    m = minimum xs
    (l, r) = break (== m) xs
    t = tail r