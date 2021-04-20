{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Glib.Quarks

main :: IO ()
main = do
	f <- gQuarkFromString "foo"
	b <- gQuarkFromString "bar"
	f' <- gQuarkFromString "foo"
	print $ f == f'
	putStrLn =<< gQuarkToString b
	print =<< gQuarkTryString "bar"
	print =<< gQuarkTryString "baz"
	print =<< gInternString "Hello, world!"
	print =<< gInternString "Good-morning!"
	print =<< gInternString "Hello, world!"
