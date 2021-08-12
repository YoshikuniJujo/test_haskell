{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Graphics.Gdk.PointsAndRectangles

main :: IO ()
main = do
	let	r1 = GdkRectangle 10 20 30 40
		r2 = GdkRectangle {
			gdkRectangleX = 20,
			gdkRectangleY = 30,
			gdkRectangleWidth = 50,
			gdkRectangleHeight = 70 }
		r3 = GdkRectangle 40 60 70 80
	print r1
	print r2
	print =<< gdkRectangleThaw r1
	rp1 <- gdkRectangleNew
	print =<< gdkRectangleFreeze rp1
	print rp1
	print =<< gdkRectangleCopy rp1

	ri :: GdkRectangleIO <- gdkRectangleNew
	print =<< gdkRectangleIntersect r1 r2 ri
	print =<< gdkRectangleFreeze ri
	print =<< gdkRectangleIntersect r1 r3 ri
	let	ru = union r1 r2
	print ru
	print . gdkRectangleEqual ru $ GdkRectangle 10 20 60 80

union :: GdkRectangle -> GdkRectangle -> GdkRectangle
union r1 r2 = runST do
	ru :: GdkRectangleST s <- gdkRectangleNew
	gdkRectangleUnion r1 r2 ru
	gdkRectangleFreeze ru
