{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import System.Directory

import Tar

main :: IO ()
main = do
	tfp_ : sd : sfps <- getArgs
	tfp <- makeAbsolute tfp_
	setCurrentDirectory sd
	tar tfp sfps
