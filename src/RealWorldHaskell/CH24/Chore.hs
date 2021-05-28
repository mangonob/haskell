module RealWorldHaskell.CH24.Chore where

import Control.Concurrent

communicate :: Int -> IO ()
communicate n = do
  m <- newEmptyMVar
  forkIO $ do
    threadDelay n
    putMVar m ()
  takeMVar m