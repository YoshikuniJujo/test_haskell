{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Data.List

import Lib

main :: IO ()
main = do
	as <- getArgs
	case as of
		[fp]	| ".png" `isSuffixOf` fp -> sample1 fp
		_ -> putStrLn "Usage: stack exec try-trochoid-exe foo.png"
