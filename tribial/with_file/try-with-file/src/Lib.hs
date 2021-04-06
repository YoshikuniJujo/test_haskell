{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.IORef
import System.IO

insane :: IO ()
insane = do
	h <- withFile "hello.txt" ReadMode pure
	putStr =<< hGetContents h

insane2 :: IO ()
insane2 = do
	handle <- newIORef stdin
	withFile "hello.txt" ReadMode \h -> writeIORef handle h
	putStr =<< hGetContents =<< readIORef handle
