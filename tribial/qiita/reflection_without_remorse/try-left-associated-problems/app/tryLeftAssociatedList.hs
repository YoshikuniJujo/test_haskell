{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import LeftAssociatedList

main :: IO ()
main = do
	print $ last helloL
	print $ last helloR
