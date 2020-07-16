{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (
	someFunc,
	module Lib2
	) where

import Lib2

someFunc :: IO ()
someFunc = putStrLn "someFunc"
