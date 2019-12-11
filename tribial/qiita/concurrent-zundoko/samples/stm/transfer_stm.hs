{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

type Account = TVar Int

newAccount :: Int -> STM Account
newAccount = newTVar

withdraw :: Account -> Int -> STM ()
withdraw acc amnt = do
	bl <- readTVar acc
	writeTVar acc $ bl - amnt

deposit :: Account -> Int -> STM ()
deposit acc amnt = do
	bl <- readTVar acc
	writeTVar acc $ bl + amnt

inquiry :: Account -> STM Int
inquiry = readTVar

inquiryAll :: [Account] -> IO Int
inquiryAll accs = atomically $ sum <$> (inquiry `mapM` accs)

transfer :: Account -> Account -> Int -> IO ()
transfer a b amnt = atomically $ withdraw a amnt >> deposit b amnt

try :: IO ()
try = do
	a <- atomically $ newAccount 800
	b <- atomically $ newAccount 300
	void . forkIO $ transfer a b 500
	inquiryAll [a, b] >>= print

noDeadlock :: IO ()
noDeadlock = do
	a <- atomically $ newAccount 800
	b <- atomically $ newAccount 300
	void . forkIO $ transfer a b 500
	transfer b a 200
	print =<< atomically ((,) <$> inquiry a <*> inquiry b)
	threadDelay 500000
	print =<< atomically ((,) <$> inquiry a <*> inquiry b)
