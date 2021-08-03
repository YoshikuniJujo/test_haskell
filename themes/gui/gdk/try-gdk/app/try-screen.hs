{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen

main :: IO ()
main = do
	gdkDisplayOpen ""
	mscr <- gdkScreenGetDefault
	print mscr
