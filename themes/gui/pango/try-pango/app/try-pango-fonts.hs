{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import System.Environment
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.Fonts
import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Values

main :: IO ()
main = getArgs >>= \case
	f : st : v : _ -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
		cr <- cairoCreate s
		pl <- pangoCairoCreateLayout cr

		fd <- pangoFontDescriptionNew
		case f of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ Family f
		print =<< pangoFontDescriptionGet @Family fd
		case st of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readStyle st
		print =<< pangoFontDescriptionGet @PangoStyle fd
		case v of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readVariant v
		print =<< pangoFontDescriptionGet @PangoVariant fd

		pangoLayoutSetFontDescription pl fd
		pangoLayoutSetText pl "Hello, world!\nこんにちは、世界!\n\x1f9a5" 45

		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "no family"

readStyle :: String -> PangoStyle
readStyle "normal" = pangoStyleNormal
readStyle "oblique" = pangoStyleOblique
readStyle "italic" = pangoStyleItalic
readStyle _ = pangoStyleNormal

readVariant :: String -> PangoVariant
readVariant "normal" = pangoVariantNormal
readVariant "small-caps" = pangoVariantSmallCaps
readVariant _ = pangoVariantNormal
