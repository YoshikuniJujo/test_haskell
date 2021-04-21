{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Glib.Quarks.Internal

main :: IO ()
main = do
	let	f = gQuarkFromString "foo"
		b = gQuarkFromString "bar"
		f' = gQuarkFromString "foo"
	print $ f == f'
	putStrLn $ gQuarkToString b
	print =<< gQuarkTryString "bar"
	print =<< gQuarkTryString "baz"
	let	ih = gInternString "Hello, world!"
		ig = gInternString "Good-morning!"
		ih' = gInternString "Hello, world!"
	print $ ih == ig
	print $ ih == ih'
	putStrLn $ gUninternString ih
	putStrLn $ gUninternString ig
