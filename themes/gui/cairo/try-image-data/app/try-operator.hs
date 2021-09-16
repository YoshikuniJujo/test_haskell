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
			Draw {	drawOperator = OperatorOver,
				drawClip = [],
				drawSource = Source . PatternColor . ColorRgba
					. fromJust $ rgbaDouble 0.15 0.3 0.05 1.0,
				drawMask = MaskPaint 1 },
			Draw {	drawOperator = OperatorOver,
				drawClip = [],
				drawSource = Source . PatternColor . ColorRgba
					. fromJust $ rgbaDouble 0.3 0.15 0.05 1.0,
				drawMask = MaskFill [Rectangle 32 32 64 64] },
			Draw {	drawOperator = OperatorClear,
				drawClip = [],
				drawSource = Source . PatternColor . ColorRgba
					. fromJust $ rgbaDouble 0.15 0.05 0.3 1.0,
				drawMask = MaskFill [Rectangle 48 48 64 64] }
			] }
	makePng sr "pngs/try-operator.png"
