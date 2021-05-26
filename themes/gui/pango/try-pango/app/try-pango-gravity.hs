{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Values
import Graphics.Pango.Basic.Rendering
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
	cr <- cairoCreate s
	cairoTranslate cr 300 0
	cairoRotate cr (pi / 2)

	ctx <- pangoCairoCreateContext cr
	pangoContextSet ctx $ BaseGravity PangoGravityEast
	pangoContextSet ctx sourceHanSans
	pl <- pangoLayoutNew ctx
	cairoMoveTo cr 5 50
	pangoLayoutSet @T.Text pl "Hello, world! こんにちは、世界! あいう愛愛愛"
	pangoCairoShowLayout cr pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-gravity.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

sourceHanSans :: PangoFontDescription
sourceHanSans = runST do
	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd $ Family "Source Han Sans VF"
--	pangoFontDescriptionSet fd $ Family "Sazanami Mincho"
	pangoFontDescriptionSet fd $ Size 13
	pangoFontDescriptionFreeze fd
