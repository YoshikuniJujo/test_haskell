{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import GHC.Exts.Heap

someFunc :: IO ()
someFunc = putStrLn "someFunc"

test :: IO ()
test = do
	print =<< getClosureData someFunc
	someFunc
	print =<< getClosureData someFunc
