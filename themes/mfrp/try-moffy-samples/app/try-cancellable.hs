{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Stopgap.System.GLib.File

main :: IO ()
main = do
	fp : _ <- getArgs
	f <- newForPath fp
	print =<< loadContents f Nothing
