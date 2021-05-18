{-# LANGUAGE BangPatterns #-}

module Utils (noImplemented, time) where

import Data.Time

noImplemented :: a
noImplemented = error "no implemented"

time :: p -> IO NominalDiffTime
time f = do
  c <- getCurrentTime
  let !v = f
  d <- getCurrentTime
  return $ diffUTCTime d c