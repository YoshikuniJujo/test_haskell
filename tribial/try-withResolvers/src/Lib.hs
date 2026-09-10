{-# LANGUAGE RankNTypes #-}

module Lib where

import Control.Exception
import Control.Concurrent.MVar

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newPromise :: Exception e =>
	(forall b . (a -> IO b) -> (e -> IO b) -> IO b) -> IO a
newPromise executor = executor pure throw

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
