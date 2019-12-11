{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

main :: IO ()
main = do
	lst <- atomically $ newTVar []
	void . forkIO $ threadDelay 3000000 >>
		atomically (writeTVar lst "Hello, world!")
	chr <- atomically $ readTVar lst >>= \case
		[] -> retry
		c : _ -> pure c
	print chr
