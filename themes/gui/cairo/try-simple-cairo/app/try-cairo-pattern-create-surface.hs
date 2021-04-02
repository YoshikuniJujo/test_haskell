{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT.Basic
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces

main :: IO ()
main = readImage "data/HaskellLogo.png" >>= \case
	Right (ImageRGBA8 i) -> do
		let i' = juicyRGBA8ToCairoArgb32 i
		putStrLn $ show (imageWidth i) ++ " " ++ show (imageHeight i)
		sr0 <- cairoImageSurfaceCreate cairoFormatArgb32 256 256
		sr1 <- cairoImageSurfaceCreateForCairoImage $ CairoImageArgb32 i'
		cr <- cairoCreate sr0
		pt <- cairoPatternCreateForSurface sr1

		print sr1
		print =<< cairoPatternGetSurface pt

		cairoTranslate cr 64 64
		cairoSetSource cr pt
		cairoPaint cr
		cairoImageSurfaceGetCairoImage sr0 >>= \case
			CairoImageArgb32 ci ->
				writePng "try-cairo-pattern-create-surface.png" $ cairoArgb32ToJuicyRGBA8 ci
			_ -> error "never occur"
	_ -> error "bad"
