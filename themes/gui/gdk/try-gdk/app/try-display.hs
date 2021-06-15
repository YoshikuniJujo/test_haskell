{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment

import Graphics.Gdk.General

main :: IO ()
main = do
	args <- join $ gdkInit <$> getProgName <*> getArgs
	print args
