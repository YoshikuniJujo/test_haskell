{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Data.ImageData
import Trial.TryCairo
import Trial.MakePng

import Data.CairoImage.Internal

main :: IO ()
main = (`makePng` "pngs/try-fill-rule.png") =<< makeSurface Surface {
	surfaceBase = SurfaceBaseBlank {
		surfaceBaseWidth = 768, surfaceBaseHeight = 896 },
	surfaceClips = [
		Clip {	clipBounds = [],
			clipDraws = concat [
				draw FillRuleWinding 0 0,
				draw FillRuleEvenOdd 300 0 ] },
		Clip {	clipBounds = [Bound FillRuleWinding $ paths 0 300],
			clipDraws = [Draw {
				drawOperator = OperatorOver,
				drawSource = Source . PatternSolid . ColorRgba . fromJust
					$ rgbaDouble 0.7 0.7 0.7 1,
				drawMask = MaskPaint 1 }] },
		Clip {	clipBounds = [Bound FillRuleEvenOdd $ paths 300 300],
			clipDraws = [Draw {
				drawOperator = OperatorOver,
				drawSource = Source . PatternSolid . ColorRgba . fromJust
					$ rgbaDouble 0.7 0.7 0.7 1,
				drawMask = MaskPaint 1 }] } ] }

draw :: FillRule -> Double -> Double -> [Draw 'Rgba]
draw fr x y = [
	Draw {	drawOperator = OperatorOver,
		drawSource = Source . PatternSolid . ColorRgba . fromJust
			$ rgbaDouble 0.7 0.7 0.7 1,
		drawMask = MaskFill fr $ paths x y },
	Draw {	drawOperator = OperatorOver,
		drawSource = Source . PatternSolid . ColorRgba . fromJust
			$ rgbaDouble 0.2 0.6 0.1 1,
		drawMask = MaskStroke (LineWidth 4)
			(LineDash [] 0) LineCapButt
			(LineJoinMiter 10) $ paths x y } ]

paths :: Double -> Double -> [Path]
paths x y = [
	PathTransform $ Transform 0.5 0 0 0.5 (- 30 + x) y,
	MoveTo 100 300, LineTo 200 500, LineTo 400 400, LineTo 500 200,
	LineTo 400 100, LineTo 200 100, LineTo 200 300, LineTo 500 400,
	LineTo 500 300, LineTo 300 250, LineTo 250 150, LineTo 350 50,
	LineTo 450 50, LineTo 250 400, ClosePath ]
