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
main = do
	(`makePng` "pngs/try-gradient-linear.png") =<< makeSurface Surface {
		surfaceBase = SurfaceBaseBlank {
			surfaceBaseWidth = 896, surfaceBaseHeight = 896 },
		surfaceClips = [
			clip1 PatternExtendPad 0 0,
			clip1 PatternExtendNone 400 0,
			clip1 PatternExtendRepeat 0 400,
			clip1 PatternExtendReflect 400 400 ] }
	(`makePng` "pngs/try-gradient-radial.png") =<< makeSurface Surface {
		surfaceBase = SurfaceBaseBlank {
			surfaceBaseWidth = 896, surfaceBaseHeight = 896 },
		surfaceClips = [
			clip2 PatternExtendPad 0 0,
			clip2 PatternExtendNone 400 0,
			clip2 PatternExtendRepeat 0 400,
			clip2 PatternExtendReflect 400 400 ] }

clip1, clip2 :: PatternExtend -> Double -> Double -> Clip 'Rgba
clip1 pe x y = Clip {
	clipBounds = [
		Bound FillRuleWinding [Rectangle (x + 50) (y + 50) 350 350]],
	clipDraws = [
		Draw {	drawOperator = OperatorOver,
			drawSource = Source
				. PatternNonSolid PatternFilterGood pe
					(Transform 1 0 0 1 (- x) (- y))
				$ PatternGradient
					(GradientFrameLinear
						(100, 100) (300, 300))
					[(0.1, green), (0.9, red), (0.5, yellow)],
			drawMask = MaskPaint 1 } ] }

clip2 pe x y = Clip {
	clipBounds = [
		Bound FillRuleWinding [Rectangle (x + 50) (y + 50) 350 350]],
	clipDraws = [
		Draw {	drawOperator = OperatorOver,
			drawSource = Source
				. PatternNonSolid PatternFilterGood pe
					(Transform 1 0 0 1 (- x) (- y))
				$ PatternGradient
					(GradientFrameRadial circle1 circle2)
					[(0.1, green), (0.9, red), (0.5, yellow)],
			drawMask = MaskPaint 1 } ] }

red, green, yellow :: SurfaceTypeColor 'Rgba
red = ColorRgba . fromJust $ rgbaDouble 0.6 0.2 0.1 1
green = ColorRgba . fromJust $ rgbaDouble 0.2 0.6 0.1 1
yellow = ColorRgba . fromJust $ rgbaDouble 0.7 0.7 0.1 1

circle1, circle2 :: Circle
circle1 = Circle (200, 300) 50
circle2 = Circle (300, 300) 200
