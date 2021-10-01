{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

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
draw img = (`makePng` "pngs/try-surface-source.png") =<< makeSurface Surface {
	surfaceBase = SurfaceBaseBlank 576 512,
	surfaceClips = [clip img] }

clip :: SurfaceBase 'Rgba -> Clip 'Rgba
clip img = Clip {
	clipBounds = [],
	clipDraws = [Draw {	
		drawOperator = OperatorOver,
		drawSource = Source . PatternNonSolid PatternFilterGood
				PatternExtendNone (Transform 1 0 0 1 112 16)
			$ PatternSurface Surface {
				surfaceBase = img, surfaceClips = [] },
		drawMask = MaskFill [Arc 288 256 192 0 (2 * pi)] }] }
