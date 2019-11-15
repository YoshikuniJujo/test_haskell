{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef

data Account = Account (MVar ()) (IORef Int)

newAccount :: Int -> IO Account
newAccount bl = Account <$> newMVar () <*> newIORef bl

withdraw :: Account -> Int -> IO ()
withdraw (Account m acc) amnt = do
	takeMVar m
	bl <- readIORef acc
	threadDelay 100000
	writeIORef acc $ bl - amnt
	putMVar m ()

deposit :: Account -> Int -> IO ()
deposit (Account m acc) amnt = do
	takeMVar m
	threadDelay 50000
	bl <- readIORef acc
	threadDelay 100000
	writeIORef acc $ bl + amnt
	putMVar m ()

inquiry :: Account -> IO Int
inquiry (Account _ r) = readIORef r

main :: IO ()
main = do
	acc <- newAccount 1000
	void . forkIO $ withdraw acc 500
	void . forkIO $ deposit acc 500
	threadDelay 1000000
	print =<< inquiry acc
