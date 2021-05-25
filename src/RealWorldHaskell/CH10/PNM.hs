module RealWorldHaskell.CH10.PNM where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isSpace)

data Greymap = Greymap
  { greyWidth :: Int,
    greyHeight :: Int,
    greyMax :: Int,
    greyData :: B.ByteString
  }
  deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

-- 匹配
matchHeader :: B.ByteString -> B.ByteString -> Maybe B.ByteString
matchHeader prefix xs
  | C.isPrefixOf prefix xs = Just $ B.drop (B.length prefix) xs
  | otherwise = Nothing

-- 读取自然数
getNat :: B.ByteString -> Maybe (Int, B.ByteString)
getNat = C.readInt

getBytes :: Int -> B.ByteString -> Maybe (B.ByteString, B.ByteString)
getBytes l bs =
  let (left, right) = B.splitAt (fromIntegral l) bs
   in if (not . B.null) left
        then Just (left, right)
        else Nothing

parseP5 :: B.ByteString -> Maybe (Greymap, B.ByteString)
parseP5 bs = do
  body <- matchHeader (C.pack "P5") bs
  (width, body) <- getNat $ C.dropWhile isSpace body
  (height, body) <- getNat $ C.dropWhile isSpace body
  (maxGrey, body) <- getNat $ C.dropWhile isSpace body
  guard $ maxGrey <= 255
  (n, body) <- getBytes 1 body
  guard $ n == C.pack "\n"
  (bitmap, tail) <- getBytes (width * height) body
  return ((Greymap width height maxGrey bitmap), tail)
  where
    guard p = if p then return () else Nothing

l_hires :: IO (Maybe (Greymap, C.ByteString))
l_hires = do
  file <- B.readFile "src/RealWorldHaskell/CH10/resource/l_hires.pgm"
  return $ parseP5 file