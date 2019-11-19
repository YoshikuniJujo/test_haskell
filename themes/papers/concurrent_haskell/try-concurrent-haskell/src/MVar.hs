module MVar where

import Control.Monad
import Control.Concurrent

forkTakeMVar :: Show a => MVar a -> IO ()
forkTakeMVar v = void . forkIO $ takeMVar v >>= putStrLn . ("Value: " ++) . show

forkPutMVar :: MVar a -> a -> IO ()
forkPutMVar v x = void . forkIO $ putMVar v x
