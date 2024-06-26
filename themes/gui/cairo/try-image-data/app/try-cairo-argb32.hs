{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Angle
import Data.Color
import Data.ImageData
import System.Environment
import Trial.TryCairo
import Trial.MakePng
import Trial.ReadPng

main :: IO ()
main = do
	[fp] <- getArgs
	maybe (putStrLn "bad image file") draw =<< readSurfaceBaseArgb32 fp

draw :: SurfaceBase 'Rgba -> IO ()
draw img = (`makePng` "pngs/try-cairo-argb32.png") =<< makeSurface Surface {
	surfaceBase = img,
	surfaceClips = [
		Clip {	clipBounds = [Bound FillRuleWinding [Rectangle 8 8 752 880]],
			clipDraws = [
				Draw {	drawOperator = OperatorOver,
					drawSource = Source
						. PatternSolid . ColorRgba . fromJust $ rgbaDouble 0.15 0.3 0.05 0.3,
					drawMask = MaskPaint 1 } ] },
		Clip {	clipBounds = [],
			clipDraws = [
				Draw {	drawOperator = OperatorOver,
					drawSource = Source
						. PatternSolid . ColorRgba . fromJust $ rgbaDouble 0.05 0.1 0.025 0.3,
					drawMask = MaskStroke (LineWidth 64) (LineDash [] 0) LineCapButt (LineJoinMiter 1) [
						PathTransform $ rot (- pi / 12) (- 68) 128,
						Rectangle 96 96 544 544 ] },
				Draw {	drawOperator = OperatorOver,
					drawSource = Source
						. PatternSolid . ColorRgba . fromJust $ rgbaDouble 0.5 0.2 0.1 0.3,
					drawMask = MaskStroke (LineWidth 64) (LineDash [] 0) LineCapButt (LineJoinMiter 100) [
						Arc 320 96 96 (Degree 0) (Degree 120) ] },
				Draw {	drawOperator = OperatorOver,
					drawSource = Source
						. PatternSolid . ColorRgba . fromJust $ rgbaDouble 0.5 0.2 0.1 0.3,
					drawMask = MaskStroke (LineWidth 64) (LineDash [] 0) LineCapButt (LineJoinMiter 100) [
						ArcNegative 512 384 96 (Degree 0) (Degree 120) ] } ] },
		Clip {	clipBounds = [
				Bound FillRuleWinding [Rectangle 128 512 128 128],
				Bound FillRuleWinding [Arc 224 512 64 (Degree 0) (Degree 360)] ],
			clipDraws = [
				Draw {	drawOperator = OperatorOver,
					drawSource = Source
						. PatternSolid . ColorRgba . fromJust $ rgbaDouble 0.15 0.1 0.25 0.3,
					drawMask = MaskPaint 1 } ] } ] }

rot :: Double -> Double -> Double -> Transform
rot a = Transform (cos a) (- sin a) (sin a) (cos a)
