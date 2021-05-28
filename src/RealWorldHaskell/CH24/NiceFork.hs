module RealWorldHaskell.CH24.NiceFork where

import Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    modifyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
    threadDelay,
    tryTakeMVar,
  )
import Control.Exception (SomeException, try)
import Control.Monad (join)
import qualified Data.Map as M

data ThreadStatus = Running | Finished | Threw SomeException deriving (Show)

(??) :: Maybe a -> Maybe a -> Maybe a
Just x ?? _ = Just x
Nothing ?? y = y

newManager :: IO ThreadManager
newManager = fmap ThreadManager $ newMVar M.empty

forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (ThreadManager mgr) body = do
  modifyMVar mgr $ \old -> do
    state <- newEmptyMVar
    tid <- forkIO $ do
      result <- try body
      putMVar state (either Threw (const Finished) result)
    return (M.insert tid state old, tid)

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (ThreadManager mgr) tid = do
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m, Nothing)
      Just st ->
        tryTakeMVar st >>= \mst -> case mst of
          Nothing -> return (m, Just Running)
          Just sth -> return (M.delete tid m, Just sth)

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (ThreadManager mgr) tid = do
  join . modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, return Nothing)
      (Just st, m') -> (m', Just `fmap` takeMVar st)

waitAll :: ThreadManager -> IO ()
waitAll (ThreadManager mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
  where
    elems m = return (M.empty, M.elems m)

newtype ThreadManager
  = ThreadManager (MVar (M.Map ThreadId (MVar ThreadStatus)))
  deriving (Eq)

sleep :: RealFrac a => a -> IO ()
sleep s = threadDelay $ floor $ s * 1000000

waitAllExample :: IO ()
waitAllExample = do
  mgr <- newManager
  mapM (\i -> forkManaged mgr (routine i)) [1 .. 100]
  waitAll mgr
  where
    routine n = do
      sleep $ n * 0.02
      putStrLn $ show n

waitOneExample :: IO ()
waitOneExample = do
  mgr <- newManager
  tid <- forkManaged mgr $ do
    sleep 1
    putStrLn "Thread ended"
  waitFor mgr tid
  return ()