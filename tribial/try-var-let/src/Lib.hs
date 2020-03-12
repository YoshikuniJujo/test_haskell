{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Monad
import Data.Foldable
import Control.Concurrent
import Data.IORef

{-

for (var i = 0; i < 3; i ++) {
	setTimeout(() => console.log(i), 1000);
}

-}

act :: IO ()
act = loop =<< newIORef (0 :: Int) where
	loop i = readIORef i >>= \n -> when (n < 3) do
		void . forkIO $ do
			threadDelay 1000
			print =<< readIORef i
		modifyIORef i (+ 1)
		loop i

{-

for (let i = 0; i < 3; i ++) {
	setTimeout(() => console.log(i), 1000);
}

-}

act' :: IO ()
act' = for_ [0 :: Int .. 2] \i -> forkIO $ do
	threadDelay 1000
	print i
