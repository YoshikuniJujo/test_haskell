{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent
import System.IO

main :: IO ()
main = let
	loop ch = hPutChar stdout ch >> loop ch in
	forkIO (loop 'a') >> loop 'z'
