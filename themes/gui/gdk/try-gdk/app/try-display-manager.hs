{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment
import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplayManager
import Graphics.Gdk.GdkDisplay

main :: IO ()
main = do
	dm <- gdkDisplayManagerGet
	print dm
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	d <- gdkDisplayManagerGetDefaultDisplay dm
	print d
	print $ gdkDisplayGetName d
