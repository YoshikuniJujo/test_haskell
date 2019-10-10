{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import PicoScheme

main :: IO ()
main = do
	interact picoScheme
	putStrLn ""
