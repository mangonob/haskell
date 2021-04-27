module Main where

import Data.ByteString.Lazy as B
import System.Environment

main :: IO ()
main = do
  (source : dest : _) <- getArgs
  mv source dest

mv :: FilePath -> FilePath -> IO ()
mv source dest = do
  contents <- B.readFile source
  B.writeFile dest contents