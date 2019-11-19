{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import System.IO

main :: IO ()
main = do
	void . forkIO $ hPutStr stdout "Hello"
	hPutStr stdout " world\n"
