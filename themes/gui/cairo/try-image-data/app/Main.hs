{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Data.ImageData
import Trial.TryCairo
import Trial.MakePng

main :: IO ()
main = do
	sr <- drawSurface Surface {
		sfcWidth = 128,
		sfcHeight = 128,
		sfcTrans = Transform 1 0 0 1 0 0,
		sfcSource = Source
			. PatternColor . ColorRgba . fromJust $ rgbaDouble 0.4 0.8 0.2 1.0,
		sfcMask = MaskFill $ Rectangle 16 16 96 96 }
	makePng sr "pngs/simple.png"
