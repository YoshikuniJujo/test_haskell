{-# LANGUAGE RankNTypes #-}

module Lib where

import Control.Exception
import Control.Concurrent.MVar

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newPromise :: Exception e =>
	((a -> IO ()) -> (e -> IO ()) -> IO ()) -> IO a
newPromise executor = do
	v <- newEmptyMVar
	executor (putMVar v) throw
	readMVar v

withResolvers :: Exception e => IO (IO a, a -> IO (), e -> IO b)
withResolvers = do
	v <- newEmptyMVar
	pure (readMVar v, putMVar v, throw)

baz :: (forall b . (a -> IO b) -> IO b) -> IO a
baz f = f pure

foo :: ((a -> IO a) -> IO a) -> IO a
foo executor = executor pure

bar :: (Int -> IO b) -> IO b
bar f = f 8
