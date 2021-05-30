module RealWorldHaskell.CH24.LockHierarchy where

import Control.Concurrent (MVar, forkIO, modifyMVar_, newMVar, yield)

nestedModification :: (Num a1, Num a2) => MVar a2 -> MVar a1 -> IO ()
nestedModification outer inner = do
  modifyMVar_ outer $ \x -> do
    -- yield
    modifyMVar_ inner $ \y -> return (y + 1)
    return (x + 1)
  putStrLn "done"

main :: IO ()
main = do
  a <- newMVar 1
  b <- newMVar 2
  forkIO $ nestedModification a b
  forkIO $ nestedModification b a
  return ()
