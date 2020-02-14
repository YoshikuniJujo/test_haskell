{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Timeout (timeout) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Bool

timeout :: Int -> IO x -> IO a -> IO (Maybe a)
timeout n pk rd = do
	(r, t) <- atomically $ (,) <$> newTVar False <*> newTVar False
	void . forkOS $ pk >> atomically (writeTVar r True)
	void . forkOS $ threadDelay n >> atomically (writeTVar t True)
	atomically $ check =<< (||) <$> readTVar r <*> readTVar t
	bool (pure Nothing) (Just <$> rd) =<< atomically (readTVar r)
