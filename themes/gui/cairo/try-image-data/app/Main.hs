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
		sfcWidth = 768,
		sfcHeight = 896,
		surfaceDraws = [
			Draw {	drawTrans = Transform 1 0 0 1 0 0,
				drawSource = Source
					. PatternColor . ColorRgba . fromJust $ rgbaDouble 0.15 0.3 0.05 1.0,
				drawMask = MaskPaint 1 },
			Draw {	drawTrans = Transform 1 0 0 1 0 0,
				drawSource = Source
					. PatternColor . ColorRgba . fromJust $ rgbaDouble 0.05 0.1 0.025 1.0,
				drawMask = MaskStroke (LineWidth 64) (LineJoinMiter 1) $ Rectangle 96 96 544 544 }
			] }
	makePng sr "pngs/simple.png"
