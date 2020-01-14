{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import DiffMonoid

main :: IO ()
main = do
	print $ myLast helloLString
	print $ myLast helloRString
