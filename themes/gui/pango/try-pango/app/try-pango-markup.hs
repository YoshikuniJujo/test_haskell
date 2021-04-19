{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
	cr <- cairoCreate s

	pl <- pangoCairoCreateLayout cr
	pangoLayoutSetMarkup pl "Hel<b>lo, <i>world!\nこんにちは、</i></b>世界!"
	pangoCairoShowLayout cr pl
	
	cairoMoveTo cr 0 40
	pangoLayoutSetMarkup pl "x<sup>2</sup> y<sub>3</sub>"
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 70
	pangoLayoutSetMarkup pl "<big>Hello!</big> <small>Hello!</small>"
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 90
	pangoLayoutSetMarkup pl "Hello, <s>world!</s>"
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 110
	pangoLayoutSetMarkup pl "<tt>Hello,</tt> <u>world!</u>"
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 140
	pangoLayoutSetMarkup pl
		"<span foreground=\"blue\" size=\"x-large\">Blue text</span> is <i> cool</i>!"
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 160
	pangoLayoutSetMarkup pl "&#169;Yoshikuni Jujo"
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 180
	pangoLayoutSetMarkup pl "hello <span font=\"Sans Italic 12\">hello</span>"
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 200
	pangoLayoutSetMarkup pl "<span face=\"mikachan_o\">こんにちは、</span>世界!"
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 230
	pangoLayoutSetMarkup pl $
		"<span size=\"12800\">hello</span> <span size=\"15360\">hello</span> " <>
		"<span size=\"17920\">hello</span>"
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 250
	pangoLayoutSetMarkup pl $
		"<span style=\"normal\">hello</span> <span style=\"oblique\">hello</span> " <>
		"<span style=\"italic\">hello</span>"
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 270
	pangoLayoutSetMarkup pl $
		"<span face=\"Source Han Sans VF\">" <>
		"<span weight=\"ultralight\">hello</span> " <>
		"<span weight=\"light\">hello</span> " <>
		"<span weight=\"normal\">hello</span> " <>
		"<span weight=\"bold\">hello</span> " <>
		"<span weight=\"ultrabold\">hello</span> " <>
		"<span weight=\"heavy\">hello</span>" <>
		"</span>"
	pangoCairoShowLayout cr pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-markup.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
