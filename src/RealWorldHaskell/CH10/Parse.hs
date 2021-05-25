module RealWorldHaskell.CH10.Parse where

import Control.Monad.State
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (chr, isDigit, isSpace)
import Data.Int (Int64)
import Data.Word (Word8)
import RealWorldHaskell.CH10.PNM

data ParseState = ParseState
  { -- 剩余字节
    string :: B.ByteString,
    -- 当前已读入偏移量
    offset :: Int64
  }
  deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Parse a
betterParse = undefined

type Parse a = StateT ParseState (Either String) a

identity :: a -> Parse a
identity a = StateT (\s -> Right (a, s))

parse :: Parse a -> B.ByteString -> Either String a
parse parser bs = case runStateT parser (ParseState bs 0) of
  Left e -> Left e
  Right (result, _) -> Right result

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset ps offset = ps {offset = offset}

parseByte :: Parse Word8
parseByte = do
  s <- get
  case B.uncons (string s) of
    Nothing -> StateT $ \s -> Left ("no more input at position: " ++ show (offset s))
    Just (b, r) -> do
      put (ParseState r ((offset s) + 1))
      return b

parseBytes :: Int -> Parse C.ByteString
parseBytes 0 = return C.empty
parseBytes n = do
  byte <- parseByte
  bytes <- parseBytes (n - 1)
  return $ B.cons byte bytes

w2c :: (Integral a, Num a) => a -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = fmap fst . B.uncons . string <$> get

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = do
  pass <- fmap p <$> peekByte
  case pass of
    Just True -> do
      b <- parseByte
      (b :) <$> parseWhile p
    _ -> return []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = do
  digits <- parseWhileWith w2c isDigit
  if null digits
    then StateT $ return $ Left "no more input"
    else return (read digits :: Int)

skipSpaces :: Parse ()
skipSpaces = do
  _ <- parseWhileWith w2c isSpace
  return ()

assert :: Bool -> String -> Parse ()
assert True _ = return ()
assert False reason = StateT $ return $ Left reason

-- 使用StateT重构（存在可优化的性能问题）
parseRawPGM :: Parse Greymap
parseRawPGM = do
  header <- parseWhileWith w2c (not . isSpace)
  skipSpaces
  assert (header == "P5") "invalid raw header"
  width <- parseNat
  skipSpaces
  height <- parseNat
  skipSpaces
  maxGrey <- parseNat
  parseByte
  bitmap <- parseBytes (width * height)
  return $ Greymap width height maxGrey bitmap