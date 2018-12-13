{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import System.Directory

import Tar

main :: IO ()
main = do
	tfp_ : dfp : _ <- getArgs
	tfp <- makeAbsolute tfp_
	setCurrentDirectory dfp
	untar tfp
