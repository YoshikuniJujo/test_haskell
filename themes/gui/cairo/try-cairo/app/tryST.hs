{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.Primitive
import Control.Monad.ST
import Data.CairoImage.Internal
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces

import Paths_try_cairo
import Parts (checkPattern, readArgb32, writeArgb32)

main :: IO ()
main = do
	logo <- CairoImageArgb32 <$>
		(readArgb32 =<< getDataFileName "HaskellLogo.png")

	putStrLn "*** TEST ARGB 32 BEGIN ***"

	either error (writeArgb32 "HaskellLogoRotated.png") $ runST $ draw logo

	putStrLn "*** TEST ARGB 32 END ***"

draw :: PrimMonad m => CairoImage -> m (Either String Argb32)
draw logo = do
	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 256 256
	cr <- cairoCreate sfc0
	checkPattern cr 256 256
	sfc <- cairoImageSurfaceCreateForCairoImage logo
	ptn <- cairoPatternCreateForSurface sfc
	cairoTranslate cr 128 ((256 - sqrt 2 * 128) / 2)
	cairoRotate cr (pi / 4)
	cairoSetSource cr ptn
	cairoPaint cr
	(<$> cairoImageSurfaceGetCairoImage sfc0) \case
		CairoImageArgb32 i -> Right i
		_ -> Left "image format error"
