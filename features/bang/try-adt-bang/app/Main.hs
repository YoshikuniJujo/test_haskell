{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Lib
import Summary

main :: IO ()
main = do
	print =<< checkWaldo (waldo $ Waldo "hello" "world")
	print =<< checkFred (fred $ Fred "hello" "world")
	print =<< (Summary.checkFoo $! foo (Foo ""))
	print =<< (Summary.checkBaz $! baz (Summary.Baz Empty))
