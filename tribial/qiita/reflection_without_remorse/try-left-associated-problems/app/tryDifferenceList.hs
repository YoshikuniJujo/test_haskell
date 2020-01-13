{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import DifferenceList

main :: IO ()
main = do
	print $ last helloLString
	print $ last helloRString
