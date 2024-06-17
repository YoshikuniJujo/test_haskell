{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM

main :: IO ()
main = do
	r <- atomically do
		v <- newTVar "foo"
		readTVar v
	print r
	putStrLn "Hello, STM!"
