{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Data.ImageData
import Trial.TryCairo
import Trial.MakePng

main :: IO ()
main = (`makePng` "pngs/try-line.png") =<< makeSurface Surface {
	surfaceBase = SurfaceBaseBlank {
		surfaceBaseWidth = 768, surfaceBaseHeight = 896 },
	surfaceClips = [
		Clip {
			clipBounds = [],
			clipDraws = [
				sampleForCap LineCapButt (100, 100) (300, 100),
				sampleForCap LineCapRound (100, 150) (300, 150),
				sampleForCap LineCapSquare (100, 200) (300, 200)
				] } ] }

sampleForCap :: LineCap -> (Double, Double) -> (Double, Double) -> Draw 'Rgba
sampleForCap lc (x1, y1) (x2, y2) = Draw {
	drawOperator = OperatorOver,
	drawSource = Source
		. PatternColor . ColorRgba . fromJust $ rgbaDouble 0 0 0 1,
	drawMask = MaskStroke (LineWidth 32) lc (LineJoinMiter 10)
		[MoveTo x1 y1, LineTo x2 y2] }
