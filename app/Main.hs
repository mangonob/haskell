module Main where

import Data.Char
import System.Directory
import System.Environment
import System.IO

main = do
  (command : args) <- getArgs
  case command of
    "add" -> add args
    "view" -> view args
    "remove" -> remove args
    _ -> return ()

add :: [String] -> IO ()
add (name : text : _) = appendFile name text
add _ = return ()

view :: [String] -> IO ()
view (name : _) = do
  contents <- readFile name
  putStr $ unlines $ zipWith ordLine [0 ..] $ lines contents
  where
    ordLine i l = show i ++ " - " ++ l
view _ = return ()

remove :: [String] -> IO ()
remove (name : no : _) = do
  contents <- readFile name
  (tName, tHandle) <- openTempFile "." "temp"
  let num = read no :: Integer
      newContents = unlines $ filterWithIndex (\i _ -> i /= num) $lines contents
  hPutStr tHandle newContents
  hClose tHandle
  removeFile name
  renameFile tName name
  where
    onlyLine (_, l) = l
remove _ = return ()

filterWithIndex :: (Integer -> a -> Bool) -> [a] -> [a]
filterWithIndex _ [] = []
filterWithIndex f xs = map (\(_, l) -> l) $ filter (\(i, l) -> f i l) $ zip [0 ..] xs