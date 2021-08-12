{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Gdk.PointsAndRectangles

main :: IO ()
main = do
	print $ GdkRectangle 123 456 789 987
	print $ GdkRectangle {
		gdkRectangleX = 987,
		gdkRectangleY = 654,
		gdkRectangleWidth = 321,
		gdkRectangleHeight = 123 }
