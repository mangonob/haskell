module RealWorldHaskell.CH04.Adler32 () where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.Char (ord)

adler32' :: [Char] -> Int
adler32' xs =
  let (a, b) = foldl reducer (1, 0) xs
   in shiftL b 16 .|. a
  where
    base = 65521
    reducer (a, b) x =
      let a' = mod (a + ord x .&. 0xff) base
          b' = mod (a' + b) base
       in (a', b')