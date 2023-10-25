{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Data.ImageData
import Trial.TryCairo
import Trial.MakePng

import Data.CairoImage.Internal

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
						$ PatternMesh [quad1, quad2, quad1', quad3, quad4],
					drawMask = MaskPaint 1 } ] } ] }

quad1, quad1', quad2, quad3, quad4 :: PatternQuadrangle
quad1 = PatternQuadrangle
	(meshPaths1 100 50) (MeshColors red yellow green blue)
	meshControlPoints1

quad1' = PatternQuadrangle
	(meshPaths1 100 300) (MeshColors red yellow green blue)
	(meshControlPoints2 100 300)

quad2 = PatternQuadrangle
	(meshPaths1 300 50) (MeshColors yellow white black green) 
	meshControlPoints1

quad3 = PatternQuadrangle
	(meshPaths2 100 550) (MeshColors red yellow green blue)
	meshControlPoints1

quad4 = PatternQuadrangle
	(meshPaths3  300 550) (MeshColors yellow white black green)
	meshControlPoints1

meshPaths1 :: Double -> Double -> MeshPaths
meshPaths1 x y = MeshPaths
	(MeshMoveTo l t) (MeshLineTo r t) (MeshLineTo r b) (MeshLineTo l b)
	MeshCloseTo
	where t = y; b = y + 200; l = x; r = x + 200

meshPaths2 :: Double -> Double -> MeshPaths
meshPaths2 x y = MeshPaths
	(MeshMoveTo l t) (MeshLineTo r t) (bezier1 r t) (MeshLineTo l b)
	MeshCloseTo
	where t = y; b = y + 200; l = x; r = x + 200

meshPaths3 :: Double -> Double -> MeshPaths
meshPaths3 x y = MeshPaths
	(MeshMoveTo l t) (MeshLineTo r t) (MeshLineTo r b) (MeshLineTo l b)
	(MeshCloseCurveTo (Point (l - 200) (b - 50)) (Point (l + 200) (t + 50)))
	where t = y; b = y + 200; l = x; r = x + 200

bezier1 :: Double -> Double -> MeshLineCurveTo
bezier1 x y = MeshCurveTo
	(Point (x + 200) (y + 50))
	(Point (x - 200) (y + 150))
	(Point x (y + 200))

meshColors1 :: MeshColors
meshColors1 = MeshColors red yellow green blue

red, green, blue, yellow, black, white :: Rgba Double
red = fromJust $ rgbaDouble 0.6 0.2 0.1 1
green = fromJust $ rgbaDouble 0.2 0.6 0.1 1
blue = fromJust $ rgbaDouble 0.2 0.2 0.6 1
yellow = fromJust $ rgbaDouble 0.7 0.7 0.1 1
black = fromJust $ rgbaDouble 0.2 0.2 0.1 1
white = fromJust $ rgbaDouble 0.8 0.8 0.7 1

meshControlPoints1 :: MeshControlPoints
meshControlPoints1 = MeshControlPoints Nothing Nothing Nothing Nothing

meshControlPoints2 :: Double -> Double -> MeshControlPoints
meshControlPoints2 x y = MeshControlPoints
	(Just $ Point l t) (Just $ Point r t) (Just $ Point r b) (Just $ Point l b)
	where
	t = y + 180; b = y + 190; l = x + 10; r = x + 20
