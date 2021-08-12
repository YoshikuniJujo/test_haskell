{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Graphics.Gdk.PointsAndRectangles

main :: IO ()
main = do
	let	r1 = GdkRectangle 123 456 789 987
		r2 = GdkRectangle {
			gdkRectangleX = 987,
			gdkRectangleY = 654,
			gdkRectangleWidth = 321,
			gdkRectangleHeight = 123 }
	print r1
	print r2
	print =<< gdkRectangleThaw r1
	rp1 <- gdkRectangleNew
	print =<< gdkRectangleFreeze rp1
	print rp1
	print =<< gdkRectangleCopy rp1
