{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Applicative
import Control.Concurrent

getAsyncException :: IO (Either AsyncException String)
getAsyncException = try getLine

getAsyncCatch :: (AsyncException -> IO String) -> IO String
getAsyncCatch handler = getLine `catch` handler

sleepAsyncCatch :: IO String
sleepAsyncCatch = getAsyncCatch $ \e -> myThreadDelay 5000000 >> return (show e)

getAsyncTry :: (AsyncException -> IO String) -> IO String
getAsyncTry handler = do
	r <- getAsyncException
	case r of
		Left e -> handler e
		Right s -> return s

sleepAsyncTry :: IO String
sleepAsyncTry = getAsyncTry $ \e -> myThreadDelay 5000000 >> return (show e)

sleepAsyncMask :: IO String
sleepAsyncMask = getAsyncCatch $ \e -> mask_ $ threadDelay 5000000 >> return (show e)

sleepMask :: IO ()
sleepMask = mask_ $ threadDelay 5000000

sleepUIMask :: IO ()
sleepUIMask = uninterruptibleMask_ $ threadDelay 5000000

testMask :: IO ()
testMask = do
	end <- newChan
	tid <- forkIO $ notMask_ $ do
		(myThreadDelay 5000000) `catch`
			\(e :: AsyncException) -> print e
		writeChan end ()
	print tid
	killThread tid
	readChan end

myThreadDelay :: Int -> IO ()
myThreadDelay n = print [1 .. n `div` 100]

notMask_ :: IO a -> IO a
notMask_ = id

readLineCatch :: IO String
readLineCatch = getLine `catch` \(e :: AsyncException) -> do
	myThreadDelay 5000000
	return $ show e

readLineTry :: IO String
readLineTry = do
	r <- try getLine :: IO (Either AsyncException String)
	case r of
		Left e -> do
			myThreadDelay 5000000
			return $ show e
		Right s -> return s

retryLineCatch :: IO String
retryLineCatch = getLine `catch` \(e :: AsyncException) -> do
	myThreadDelay 1000000
	retryLineCatch

retryLineTry :: IO String
retryLineTry = do
	r <- try getLine :: IO (Either AsyncException String)
	case r of
		Left e -> do
			myThreadDelay 1000000
			retryLineTry
		Right s -> return s
