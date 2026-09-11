{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lib where

import Control.Monad
import Control.Exception
import Control.Concurrent

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newPromise :: Exception e =>
	((a -> IO ()) -> (e -> IO ()) -> IO ()) -> IO (IO a)
newPromise executor = do
	v <- newEmptyMVar
	_ <- forkIO $ executor (putMVar v) throw
	pure $ readMVar v

withResolvers :: Exception e => IO (IO (IO a), a -> IO (), e -> IO ())
withResolvers = do
	v <- newEmptyMVar
	pure (	pure $ either throw pure =<< readMVar v,
		putMVar v . Right, putMVar v . Left)

foo :: IO Int
foo = do
	join $ newPromise @SomeException \rs _rj -> rs (123 :: Int)

bar :: IO Int
bar = do
	(pr, rs, _rj) <- withResolvers @SomeException
	_ <- forkIO $ threadDelay 1000000 >> rs 123
	join pr
