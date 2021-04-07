{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT
import Graphics.Cairo.Surfaces.SvgSurfaces

main :: IO ()
main = do
	((putStrLn . cairoSvgVersionToString) `mapM_`) =<< cairoSvgGetVersions
	cairoSvgSurfaceWith "try-svg.svg" 128 128 \sr -> do
		pure (cairoSurfaceGetType sr) >>= \case
			CairoSurfaceTypeSvg -> putStrLn "CairoSurfaceTypeSvg"
			_ -> putStrLn "other type"
		cairoSvgSurfaceGetDocumentUnit sr >>= \case
			CairoSvgUnitPt -> putStrLn "CairoSvgUnitPt"
			_ -> putStrLn "other unit"
		cairoSvgSurfaceSetDocumentUnit sr CairoSvgUnitMm
		cairoSvgSurfaceRestrictToVersion sr CairoSvgVersion1_2
		cr <- cairoCreate sr
		cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
		cairoPaint cr
