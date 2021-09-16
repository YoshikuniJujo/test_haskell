{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Angle
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
			Draw {	drawClip = [[Rectangle 8 8 752 880]],
				drawSource = Source
					. PatternColor . ColorRgba . fromJust $ rgbaDouble 0.15 0.3 0.05 1.0,
				drawMask = MaskPaint 1 },
			Draw {	drawClip = [],
				drawSource = Source
					. PatternColor . ColorRgba . fromJust $ rgbaDouble 0.05 0.1 0.025 1.0,
				drawMask = MaskStroke (LineWidth 64) (LineJoinMiter 1) [
					PathTransform $ rot (- pi / 12) (- 68) 128,
					Rectangle 96 96 544 544 ] },
			Draw {	drawClip = [],
				drawSource = Source
					. PatternColor . ColorRgba . fromJust $ rgbaDouble 0.5 0.2 0.1 1.0,
				drawMask = MaskStroke (LineWidth 64) (LineJoinMiter 100) [
					Arc 320 96 96 (Degree 0) (Degree 120) ] },
			Draw {	drawClip = [],
				drawSource = Source
					. PatternColor . ColorRgba . fromJust $ rgbaDouble 0.5 0.2 0.1 1.0,
				drawMask = MaskStroke (LineWidth 64) (LineJoinMiter 100) [
					ArcNegative 512 384 96 (Degree 0) (Degree 120) ] },
			Draw { drawClip = [
					[Rectangle 128 512 128 128],
					[Arc 224 512 64 (Degree 0) (Degree 360)]],
				drawSource = Source
					. PatternColor . ColorRgba . fromJust $ rgbaDouble 0.15 0.1 0.25 1.0,
				drawMask = MaskPaint 1 }
			] }
	makePng sr "pngs/simple.png"

rot :: Double -> Double -> Double -> Transform
rot a = Transform (cos a) (- sin a) (sin a) (cos a)
