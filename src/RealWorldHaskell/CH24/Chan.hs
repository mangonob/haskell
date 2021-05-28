module RealWorldHaskell.CH24.Chan where

import Control.Concurrent (forkIO, newChan, readChan, writeChan)

chanExample :: IO ()
chanExample = do
  ch <- newChan
  forkIO $ do
    writeChan ch "hello world"
    writeChan ch "now i quit"
  readChan ch >>= print
  readChan ch >>= print