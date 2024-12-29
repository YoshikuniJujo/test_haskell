{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces

main :: IO ()
main = readImage "data/HaskellLogo.png" >>= \case
	Right (ImageRGBA8 i) -> do
		let i' = juicyRGBA8ToCairoArgb32 i
		putStrLn $ show (imageWidth i) ++ " " ++ show (imageHeight i)
		sr0 <- cairoImageSurfaceCreate CairoFormatArgb32 256 256
		sr1 <- cairoImageSurfaceCreateForCairoImage $ CairoImageArgb32 i'
		cr <- cairoCreate sr0
		cairoSetSourceSurface cr sr1 64 64
		cairoPaint cr
		cairoImageSurfaceGetCairoImage sr0 >>= \case
			CairoImageArgb32 ci ->
				writePng "try-cairo-set-source-surface.png" $ cairoArgb32ToJuicyRGBA8 ci
			_ -> error "never occur"
	_ -> error "bad"
