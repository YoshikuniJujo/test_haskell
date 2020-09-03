{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable
import Data.Bool

lastTChan :: TChan a -> STM a
lastTChan c = do
	x <- readTChan c
	bool (lastTChan c) (pure x) =<< isEmptyTChan c

tryLastTChan :: IO ()
tryLastTChan = do
	c <- atomically newTChan
	forkIO . forever $ print =<< atomically (lastTChan c)
--	forkIO . forever $ print =<< atomically (readTChan c)
	for_ [0 .. 9] \n -> do
		threadDelay 10000
		atomically $ writeTChan c n
	threadDelay 1000000
