{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Data.ImageData
import Trial.TryCairo
import Trial.MakePng

main :: IO ()
main = (`makePng` "pngs/try-mesh.png") =<< makeSurface Surface {
	surfaceBase = SurfaceBaseBlank {
		surfaceBaseWidth = 768, surfaceBaseHeight = 896 },
	surfaceClips = [
		Clip {	clipBounds = [],
			clipDraws = [
				Draw {	drawOperator = OperatorOver,
					drawSource = Source . PatternNonSolid
							PatternFilterGood PatternExtendNone
							(Transform 1 0 0 1 0 0)
						$ PatternMesh meshPaths meshColors meshControlPoints,
					drawMask = MaskPaint 1 } ] } ] }

meshPaths :: MeshPaths
meshPaths = MeshPaths
	(MeshMoveTo 100 100) (MeshLineTo 500 100)
	(MeshLineTo 500 500) (MeshLineTo 100 500) MeshCloseTo

meshColors :: MeshColors
meshColors = MeshColors
	(fromJust $ rgbaDouble 0.6 0.2 0.1 1)
	(fromJust $ rgbaDouble 0.7 0.7 0.1 1)
	(fromJust $ rgbaDouble 0.2 0.6 0.1 1)
	(fromJust $ rgbaDouble 0.2 0.2 0.6 1)

meshControlPoints :: MeshControlPoints
meshControlPoints = MeshControlPoints Nothing Nothing Nothing Nothing
