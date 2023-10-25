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
main = (`makePng` "pngs/try-surface-mask.png") =<< makeSurface Surface {
	surfaceBase = SurfaceBaseBlank {
		surfaceBaseWidth = 768, surfaceBaseHeight = 896 },
	surfaceClips = [
		Clip {	clipBounds = [],
			clipDraws = [
				Draw {	drawOperator = OperatorOver,
					drawSource = Source
						. PatternSolid . ColorRgba . fromJust $ rgbaDouble 0.15 0.3 0.05 1.0,
					drawMask = MaskAlpha maskPattern } ] } ] }

maskPattern :: Pattern 'Alpha
maskPattern = PatternNonSolid {
	patternFilter = PatternFilterGood,
	patternExtend = PatternExtendNone,
	patternMatrix = Transform 1 0 0 1 0 0,
	patternBody = PatternSurface mask }

mask :: Surface 'Alpha
mask = Surface {
	surfaceBase = SurfaceBaseBlank {
		surfaceBaseWidth = 500, surfaceBaseHeight = 500 },
	surfaceClips = [
		Clip {	clipBounds = [],
			clipDraws = [
				Draw {	drawOperator = OperatorOver,
					drawSource = Source . PatternSolid $ ColorAlpha 0.5,
					drawMask = MaskFill FillRuleWinding [Rectangle 100 100 200 200] },
				Draw {	drawOperator = OperatorOver,
					drawSource = Source . PatternSolid $ ColorAlpha 0.2,
					drawMask = MaskFill FillRuleWinding [Arc 300 150 100 0 $ 2 * pi] }
				] } ] }
