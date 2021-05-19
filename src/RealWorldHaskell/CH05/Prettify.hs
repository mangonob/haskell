{-# LANGUAGE ViewPatterns #-}

module RealWorldHaskell.CH05.Prettify where

import Data.Bits (Bits (shiftR, (.&.)))
import Data.Char (Char, ord)
import Data.List (foldr, length, lookup, map, replicate, zipWith)
import Numeric (showHex)
import Prelude hiding ((<>))

data Doc
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show)

empty :: Doc
empty = Empty

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = Text $ show d

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

char :: Char -> Doc
char c = Char c

line :: Doc
line = Line

-- 数组及对象
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close f =
  enclose open close . fsep . punctuate (char ',') . map f

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d : ds) = d <> p : punctuate p ds

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

oneChar :: Char -> Doc
oneChar ((`lookup` simpleEscapes) -> Just r) = text r
oneChar c@((`lookup` simpleEscapes) -> Nothing)
  | mustEscape c = hexEscape c
  | otherwise = char c
  where
    mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

-- 转义表
simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where
    ch a b = (a, ['\\', b])

hexEscape :: Char -> Doc
hexEscape c
  | d < 0x10000 = smallHex d
  | otherwise = astral (d - 0x10000)
  where
    d = ord c

smallHex :: Int -> Doc
smallHex x =
  let h = showHex x ""
   in text "\\u"
        <> text (replicate (4 - length h) '0')
        <> text h

astral :: Int -> Doc
astral n =
  let a = (shiftR n 10) .&. 0x3ff
      b = n .&. 0x3ff
   in smallHex (a + 0xd800) <> smallHex (b + 0xdc00)

(<>) :: Doc -> Doc -> Doc
a <> Empty = a
Empty <> a = a
x <> y = Concat x y

(</>) :: Doc -> Doc -> Doc
a </> b = a <> softline <> b

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = Union (flatten x) x

flatten :: Doc -> Doc
flatten (Concat x y) = Concat (flatten x) (flatten y)
flatten Line = Char ' '
flatten (Union x y) = flatten x
flatten other = other

fsep :: [Doc] -> Doc
fsep = fold (</>)

compact :: Doc -> String
compact x = transform [x]
  where
    transform [] = ""
    transform (Empty : ds) = transform ds
    transform (Char c : ds) = c : transform ds
    transform (Text s : ds) = s ++ transform ds
    transform (Line : ds) = '\n' : transform ds
    transform (Concat a b : ds) = transform (a : b : ds)
    transform (Union _ b : ds) = transform (b : ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where
    best col (Empty : ds) = best col ds
    best col (Char c : ds) = c : best (col + 1) ds
    best col (Text s : ds) = s ++ best (col + length s) ds
    best col (Line : ds) = '\n' : best 0 ds
    best col (Concat a b : ds) = best col (a : b : ds)
    best col (Union a b : ds) = nicest col (best col (a : ds)) (best col (b : ds))
    best _ _ = ""

    nicest col a b
      | fits (width - least) a = a
      | otherwise = b
      where
        least = min width col

    fits :: Int -> String -> Bool
    fits w xs | w < 0 = False
    fits w "" = True
    fits w ('\n' : xs) = True
    fits w (c : cs) = fits (w - 1) cs