{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (FileCache, newFileCache, len, body) where

import Control.Concurrent.STM
import System.IO.Unsafe

import qualified Data.ByteString as BS

data FileCache = FileCache FilePath (TVar CheckCache) (TVar BS.ByteString)

data CheckCache = NotCached | Cached deriving Show

instance Show FileCache where show _ = "(FileCache _ _ _)"

newFileCache :: FilePath -> IO FileCache
newFileCache fp = FileCache fp <$> newTVarIO NotCached <*> newTVarIO ""

cacheFile :: FileCache -> IO ()
cacheFile (FileCache fp cch bd) = do
	r <- atomically $ readTVar cch >>= \case
		NotCached -> do
			writeTVar cch Cached
			pure NotCached
		_ -> pure Cached
	case r of
		NotCached -> do
			c <- BS.readFile fp
			atomically $ writeTVar bd c
		_ -> pure ()

len :: FileCache -> Int
len fc@(FileCache _ _ bd) = unsafePerformIO
	$ BS.length <$> (cacheFile fc >> atomically (readTVar bd))

body :: FileCache -> BS.ByteString
body fc@(FileCache _ _ bd) =
	unsafePerformIO $ cacheFile fc >> atomically (readTVar bd)
