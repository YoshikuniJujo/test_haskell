{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Lib

main :: IO ()
main = do
	fp : _ <- getArgs
	tryField fp
