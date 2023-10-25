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
main = (`makePng` "pngs/try-line.png") =<< makeSurface Surface {
	surfaceBase = SurfaceBaseBlank {
		surfaceBaseWidth = 768, surfaceBaseHeight = 896 },
	surfaceClips = [
		Clip {
			clipBounds = [],
			clipDraws = [
				sampleForCap LineCapButt (100, 100) (300, 100),
				sampleForCap LineCapRound (100, 150) (300, 150),
				sampleForCap LineCapSquare (100, 200) (300, 200),
				sampleForCap LineCapRound (100, 250) (100, 250),

				sampleForDash (LineDash [] 0)
					LineCapButt (100, 300) (500, 300),
				sampleForDash (LineDash [64] 0)
					LineCapButt (100, 350) (500, 350),
				sampleForDash (LineDash [64] 32)
					LineCapButt (100, 400) (500, 400),

				sampleForDash (LineDash [] 0)
					LineCapRound (100, 450) (500, 450),
				sampleForDash (LineDash [64] 0)
					LineCapRound (100, 500) (500, 500),
				sampleForDash (LineDash [64] 32)
					LineCapRound (100, 550) (500, 550),
				sampleForDash (LineDash [64, 64, 0.0, 64] 0)
					LineCapRound (100, 600) (500, 600),
				sampleForDash (LineDash [64, 64, 0.0, 64] 0)
					LineCapSquare (100, 650) (500, 650),


				sampleForCurve
					(Point 100 800) (Point 200 700)
					(Point 300 900) (Point 400 800)
				] } ] }

sampleForCap :: LineCap -> (Double, Double) -> (Double, Double) -> Draw 'Rgba
sampleForCap lc (x1, y1) (x2, y2) = Draw {
	drawOperator = OperatorOver,
	drawSource = Source
		. PatternSolid . ColorRgba . fromJust $ rgbaDouble 0 0 0 1,
	drawMask = MaskStroke (LineWidth 32) (LineDash [] 0) lc (LineJoinMiter 10)
		[MoveTo x1 y1, LineTo x2 y2] }

sampleForDash :: LineDash -> LineCap -> (Double, Double) -> (Double, Double) -> Draw 'Rgba
sampleForDash ld lc (x1, y1) (x2, y2) = Draw {
	drawOperator = OperatorOver,
	drawSource = Source
		. PatternSolid . ColorRgba . fromJust $ rgbaDouble 0 0 0 1,
	drawMask = MaskStroke (LineWidth 32) ld lc (LineJoinMiter 10)
		[MoveTo x1 y1, LineTo x2 y2] }

sampleForCurve :: Point -> Point -> Point -> Point -> Draw 'Rgba
sampleForCurve (Point x0 y0) p1 p2 pe = Draw {
	drawOperator = OperatorOver,
	drawSource = Source . PatternSolid
		. ColorRgba . fromJust $ rgbaDouble 0.2 0.6 0.1 1,
	drawMask = MaskStroke (LineWidth 32) (LineDash [] 0) LineCapRound
		(LineJoinMiter 10) [MoveTo x0 y0, CurveTo p1 p2 pe] }
