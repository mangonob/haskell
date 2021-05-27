module RealWorldHaskell.CH24.Compressor where

import Codec.Compression.GZip (compress)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, handle)
import Data.ByteString.Lazy as B (readFile, writeFile)
import System.Console.Haskeline
  ( defaultSettings,
    getInputLine,
    runInputT,
  )

readLine :: String -> IO (Maybe String)
readLine xs = runInputT defaultSettings $ getInputLine xs

main :: IO ()
main = do
  line <- readLine "Enter a file to compress> "
  case line of
    Nothing -> return ()
    Just filename -> do
      handle (print :: SomeException -> IO ()) $ do
        content <- B.readFile filename
        forkIO (compressFile filename content)
        return ()
  main
  where
    compressFile path = B.writeFile (path ++ ".gz") . compress