{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

main :: IO ()
main = do
	n_ : _ <- getArgs
	let	n = read n_ :: Int
		lst = [1 .. n]
	print . head $ reverse lst
