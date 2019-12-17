{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Control.Concurrent
import Data.IORef

type Account = IORef Int

newAccount :: Int -> IO Account
newAccount = newIORef

withdraw :: Account -> Int -> IO ()
withdraw acc amnt = do
	bl <- readIORef acc
	threadDelay 100000
	writeIORef acc $ bl - amnt

deposit :: Account -> Int -> IO ()
deposit acc amnt = do
	threadDelay 50000
	bl <- readIORef acc
	threadDelay 100000
	writeIORef acc $ bl + amnt

inquiry :: Account -> IO Int
inquiry = readIORef

main :: IO ()
main = do
	acc <- newAccount 1000
	void . forkIO $ withdraw acc 500
	void . forkIO $ deposit acc 500
	threadDelay 1000000
	print =<< inquiry acc
