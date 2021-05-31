{-# LANGUAGE BangPatterns #-}

module Utils
  ( (&&&),
    (??),
    just,
    noImplemented,
    readLine,
    time,
  )
where

import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import System.Console.Haskeline
  ( defaultSettings,
    getInputLine,
    runInputT,
  )

noImplemented :: a
noImplemented = error "no implemented"

time :: p -> IO NominalDiffTime
time f = do
  c <- getCurrentTime
  let !v = f
  d <- getCurrentTime
  return $ diffUTCTime d c

(&&&) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
f &&& g = \x -> f x && g x

(??) :: Maybe a -> Maybe a -> Maybe a
Just a ?? _ = Just a
Nothing ?? x = x

just :: Maybe a -> a
just (Just x) = x
just Nothing = error "not just value"

readLine :: String -> IO (Maybe String)
readLine xs = runInputT defaultSettings $ getInputLine xs