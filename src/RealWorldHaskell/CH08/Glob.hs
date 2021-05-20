{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module RealWorldHaskell.CH08.Glob where

import Control.Monad (filterM)
import RealWorldHaskell.CH08.GlobRegex (matchesGlob)
import System.Directory
  ( doesDirectoryExist,
    doesPathExist,
    getCurrentDirectory,
    getDirectoryContents,
  )
import System.FilePath
  ( dropTrailingPathSeparator,
    replaceExtension,
    splitFileName,
    (</>),
  )

isPattern :: String -> Bool
isPattern = any (`elem` "[?*")

namesMatching :: String -> IO [String]
namesMatching pat@(isPattern -> False) = do
  isExist <- doesPathExist pat
  return $ if isExist then [pat] else []
namesMatching (splitFileName -> (dirName, "**")) = do
  dirs <- allMatches dirName "**"
  return dirs
namesMatching (splitFileName -> ("./", baseName)) = do
  cur <- getCurrentDirectory
  contents <- getDirectoryContents cur
  return $ filter (`matchesGlob` baseName) contents
namesMatching (splitFileName -> (dirName, baseName)) = do
  dirs <- namesMatching (dropTrailingPathSeparator dirName)
  (concat -> contents) <- mapM (`allMatches` baseName) dirs
  return contents

-- 是否隐藏文件夹
isHidden :: [Char] -> Bool
isHidden ('.' : xs) = True
isHidden xs = False

-- 返回目录下所有匹配的条目
allMatches :: FilePath -> String -> IO [String]
allMatches "" pat = allMatches "." pat
allMatches dir "**" = do
  (filter (not . isHidden) -> contents) <- getDirectoryContents dir
  subDirs <- filterM doesDirectoryExist $ map (dir </>) contents
  (concat -> children) <- mapM (`allMatches` "**") subDirs
  return (subDirs ++ children)
allMatches dir pat = do
  contents <- getDirectoryContents dir
  let flt = if isHidden pat then isHidden else (not . isHidden)
  return $ filter (`matchesGlob` pat) $ filter flt contents