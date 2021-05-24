{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Map as M
import RealWorldHaskell.CH13.Common
import System.Environment
import Text.Printf
import Text.Read

data Password = Password
  { userName :: String,
    password :: String,
    uid :: Integer,
    gid :: Integer,
    geocs :: String,
    homeDir :: String,
    shell :: String
  }
  deriving (Eq, Ord)

instance Show Password where
  show p =
    printf
      "%s:%s:%d:%d:%s:%s:%s"
      (userName p)
      (password p)
      (uid p)
      (gid p)
      (geocs p)
      (homeDir p)
      (shell p)

instance Read Password where
  readsPrec _ (split ":" -> [f1, f2, f3, f4, f5, f6, f7]) =
    [(Password f1 f2 (read f3) (read f4) f5 f6 f7, [])]
  readsPrec _ _ = []

type UIDMap = M.Map Integer Password

type UserMap = M.Map String Password

main :: IO ()
main = do
  (filename : _) <- getArgs
  content <- readFile filename
  mainMenu $ inputToMaps content

inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps text =
  let passwords = (concatMap success $ map readMaybe $ lines text) :: [Password]
      uids = M.fromList $ map (\x -> (uid x, x)) passwords
      users = M.fromList $ map (\x -> (userName x, x)) passwords
   in (uids, users)
  where
    success (Just p) = [p]
    success Nothing = []

mainMenu :: (UIDMap, UserMap) -> IO ()
mainMenu maps@(uids, users) = do
  putStr optionText
  optionStr <- getLine
  let option = read optionStr :: Int
  case option of
    1 -> lookUpUserName >> mainMenu maps
    2 -> lookUpUID >> mainMenu maps
    3 -> display >> mainMenu maps
    4 -> return ()
    _ -> mainMenu maps
  where
    lookUpUID = do
      putStr "UID: "
      uidStr <- getLine
      let uid' = readMaybe uidStr :: Maybe Integer
      case uid' of
        Just uid ->
          case M.lookup uid uids of
            Just p -> print p
            Nothing -> putStrLn "Not found"
        Nothing -> putStrLn "Not found"

    lookUpUserName = do
      putStr "Username: "
      userName <- getLine
      case M.lookup userName users of
        Just p -> print p
        Nothing -> putStrLn "Not found"

    display = do
      putStrLn $ unlines $ map (show . snd) (M.toList uids)
      return ()

optionText :: [Char]
optionText =
  "\npasswdmap options:\n\
  \\n\
  \1   Look up a user name\n\
  \2   Look up a UID\n\
  \3   Display entire file\n\
  \4   Quit\n\n\
  \Your selection: "