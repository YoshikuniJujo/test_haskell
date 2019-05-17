{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SleepSort (sleepSort) where

import Data.Foldable (for_)
import Control.Concurrent (
	forkIO, threadDelay, Chan, newChan, readChan, writeChan)

sleepSort :: Int -> [Int] -> IO [Int]
sleepSort mx ns = do
	c <- newChan
	_ <- forkIO $ threadDelay (mx * 1000) >> writeChan c Nothing
	for_ ns $ \n -> forkIO $ threadDelay (n * 1000) >> writeChan c (Just n)
	result c

result :: Chan (Maybe Int) -> IO [Int]
result c = maybe (return []) ((<$> result c) . (:)) =<< readChan c
