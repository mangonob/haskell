{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char
import System.Environment

main :: IO ()
main = do
  (fileName : _) <- getArgs
  bs <- B.readFile fileName
  putStrLn $ show $ sumBs bs

sumBs :: B.ByteString -> Int
sumBs (C.readInt -> Just (a, bs)) = a + sumBs (trim bs)
sumBs (C.readInt -> Nothing) = 0

trim :: B.ByteString -> B.ByteString
trim (C.uncons -> Just ((isSpace -> True), ts)) = ts
trim bs = bs

drop' :: (Ord t, Num t) => t -> [a] -> [a]
drop' n [] = []
drop' ((<= 0) -> True) xs = xs
drop' n (x : xs) = drop' (n - 1) xs

if' :: Bool -> p -> p -> p
if' True a b = a
if' False a b = b

-- 使用章节前置的函数实现
lastButOne :: [a] -> a
lastButOne xs =
  if null (drop 2 xs)
    then head xs
    else lastButOne (tail xs)

lastButOne' :: [a] -> a
lastButOne' (x : y : []) = x
lastButOne' (x : y : xs) = lastButOne' (y : xs)