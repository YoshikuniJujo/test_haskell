{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Control.Concurrent
import Data.IORef

data Account = Account (MVar ()) (IORef Int)

newAccount :: Int -> IO Account
newAccount amnt = Account <$> newMVar () <*> newIORef amnt

lock :: Account -> IO ()
lock (Account m _) = takeMVar m

unlock :: Account -> IO ()
unlock (Account m _) = putMVar m ()

withdraw :: Account -> Int -> IO ()
withdraw (Account _ a) amnt = do
	bl <- readIORef a
	writeIORef a $ bl - amnt

deposit :: Account -> Int -> IO ()
deposit (Account _ a) amnt = do
	bl <- readIORef a
	writeIORef a $ bl + amnt

inquiry :: Account -> IO Int
inquiry (Account _ a) = readIORef a

inquiryAll :: [Account] -> IO Int
inquiryAll accs = do
	lock `mapM_` accs
	sum <$> (inquiry `mapM` accs)
		<* unlock `mapM_` accs

transfer, transfer' :: Account -> Account -> Int -> IO ()
transfer a b amnt = do
	lock a
	withdraw a amnt
	unlock a
	threadDelay 500000
	lock b
	deposit b amnt
	unlock b

transfer' a b amnt = do
	lock a
	threadDelay 500000
	lock b
	withdraw a amnt
	threadDelay 500000
	deposit b amnt
	unlock a
	unlock b

try, try' :: IO ()
try = do
	a <- newAccount 800
	b <- newAccount 300
	forkIO $ transfer a b 500
	threadDelay 250000
	inquiryAll [a, b] >>= print

try' = do
	a <- newAccount 800
	b <- newAccount 300
	forkIO $ transfer' a b 500
	threadDelay 250000
	inquiryAll [a, b] >>= print

deadlock :: IO ()
deadlock = do
	a <- newAccount 800
	b <- newAccount 300
	void . forkIO $ transfer' a b 500
--	threadDelay 250000
	transfer' b a 200
