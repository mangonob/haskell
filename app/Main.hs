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