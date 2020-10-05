{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Graphics.Gdk

main :: IO ()
main = do
	as <- getArgs
	print =<< gdkInit as
