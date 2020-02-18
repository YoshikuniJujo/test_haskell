{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Time

import React
import MouseAndTime

import Field
import Handlers

main :: IO ()
main = do
	f <- openField "スリープ" [exposureMask, buttonPressMask]
	t <- getCurrentTime
	(interpret (handleDelta500000 f) (sleep 3) `runStateT` t) >>= print
	closeField f
