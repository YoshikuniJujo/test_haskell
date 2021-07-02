{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Cairo.Drawing.Regions
import Graphics.Cairo.Utilities.Types

main :: IO ()
main = do
	let	rc = CairoRectangleIntT 10 20 200 300
	print rc
	rg <- cairoRegionCreateRectangle rc
	print rg
	rcp <- cairoRectangleIntTNew
	print rcp
	print =<< cairoRegionNumRectangles rg
	cairoRegionGetRectangle rg 0 rcp
	print =<< cairoRectangleIntTFreeze rcp
	cairoRegionGetRectangle rg 1 rcp
	print =<< cairoRectangleIntTFreeze rcp
