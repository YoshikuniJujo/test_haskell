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
	s <- cairoImageSurfaceCreate cairoFormatArgb32 500 900
	cr <- cairoCreate s

	pl <- pangoCairoCreateLayout cr
	pangoLayoutSetMarkup pl "Hel<b>lo, <i>world!\nこんにちは、</i></b>世界!"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
	
	cairoMoveTo cr 0 40
	pangoLayoutSetMarkup pl "x<sup>2</sup> y<sub>3</sub>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 70
	pangoLayoutSetMarkup pl "<big>Hello!</big> <small>Hello!</small>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 90
	pangoLayoutSetMarkup pl "Hello, <s>world!</s>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 110
	pangoLayoutSetMarkup pl "<tt>Hello,</tt> <u>world!</u>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 140
	pangoLayoutSetMarkup pl
		"<span foreground=\"blue\" size=\"x-large\">Blue text</span> is <i> cool</i>!"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 160
	pangoLayoutSetMarkup pl "&#169;Yoshikuni Jujo"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 180
	pangoLayoutSetMarkup pl "hello <span font=\"Sans Italic 12\">hello</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 200
	pangoLayoutSetMarkup pl "<span face=\"mikachan_o\">こんにちは、</span>世界!"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 230
	pangoLayoutSetMarkup pl $
		"<span size=\"12800\">hello</span> <span size=\"15360\">hello</span> " <>
		"<span size=\"17920\">hello</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 250
	pangoLayoutSetMarkup pl $
		"<span style=\"normal\">hello</span> <span style=\"oblique\">hello</span> " <>
		"<span style=\"italic\">hello</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

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
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 290
	pangoLayoutSetMarkup pl $
		"<span face=\"Alegreya Sans SC\">" <>
		"<span variant=\"normal\">Hello</span> <span variant=\"smallcaps\">Hello</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 310
	pangoLayoutSetMarkup pl $
		"<span face=\"Soulcraft\">" <>
		"<span stretch=\"ultracondensed\">hello</span> " <>
		"<span stretch=\"extracondensed\">hello</span> " <>
		"<span stretch=\"condensed\">hello</span> " <>
		"<span stretch=\"semicondensed\">hello</span> " <>
		"<span stretch=\"normal\">hello</span> " <>
		"<span stretch=\"semiexpanded\">hello</span> " <>
		"<span stretch=\"expanded\">hello</span> " <>
		"<span stretch=\"extraexpanded\">hello</span> " <>
		"<span stretch=\"ultraexpanded\">hello</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 330
	pangoLayoutSetMarkup pl $
		"<span face=\"Source Han Sans\">" <>
		"<span font_features=\"normal\">0.05</span> " <>
		"<span font_features=\"zero\">0.05</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 350
	pangoLayoutSetMarkup pl $
		"<span color=\"red\">RED</span> " <>
		"<span color=\"green\">GREEN</span> " <>
		"<span color=\"blue\">BLUE</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 370
	pangoLayoutSetMarkup pl $
		"<span bgcolor=\"red\">RED</span> " <>
		"<span bgcolor=\"green\">GREEN</span> " <>
		"<span bgcolor=\"blue\">BLUE</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 390
	pangoLayoutSetMarkup pl $
		"<span alpha=\"10%\">10%</span> " <>
		"<span alpha=\"50%\">50%</span> " <>
		"<span alpha=\"100%\">100%</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 410
	pangoLayoutSetMarkup pl $
		"<span bgcolor=\"green\" bgalpha=\"1\">" <>
		"<span bgalpha=\"10%\">10%</span> " <>
		"<span bgalpha=\"50%\">50%</span> " <>
		"<span bgalpha=\"100%\">100%</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 430
	pangoLayoutSetMarkup pl $
		"<span underline=\"none\">none</span> " <>
		"<span underline=\"single\">single</span> " <>
		"<span underline=\"double\">double</span> " <>
		"<span underline=\"low\">low ggg</span> " <>
		"<span underline=\"error\">error</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 450
	pangoLayoutSetMarkup pl $
		"<span underline=\"double\">" <>
		"<span underline_color=\"red\">red</span> " <>
		"<span underline_color=\"green\">green</span> " <>
		"<span underline_color=\"blue\">blue</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 480
	pangoLayoutSetMarkup pl $
		"<span rise=\"10240\">10</span> " <>
		"<span rise=\"5012\">5</span> " <>
		"<span rise=\"0\">0</span> " <>
		"<span rise=\"-5012\">-5</span> " <>
		"<span rise=\"-10240\">-10</span> "
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 510
	pangoLayoutSetMarkup pl $
		"<span strikethrough=\"true\">" <>
		"<span strikethrough_color=\"red\">Good-bye!</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 530
	pangoLayoutSetMarkup pl $
		"<span face=\"虚無\">" <>
		"<span fallback=\"false\">こんにちは</span> " <>
		"<span fallback=\"true\">こんにちは</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 550
	pangoLayoutSetMarkup pl $
		"<span face=\"Source Han Sans\">" <>
		"<span lang=\"jp\">源ノ角ゴシック 思源黑體 思源黑体</span>\n" <>
		"<span lang=\"zh-tw\">源ノ角ゴシック 思源黑體 思源黑体</span>\n" <>
		"<span lang=\"zh\">源ノ角ゴシック 思源黑體 思源黑体</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 610
	pangoLayoutSetMarkup pl $
		"<span letter_spacing=\"0\">Hello こんにちは\n</span>" <>
		"<span letter_spacing=\"5120\">Hello こんにちは\n</span>" <>
		"<span letter_spacing=\"10240\">Hello こんにちは</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 670
	pangoLayoutSetMarkup pl $
		"<span gravity=\"south\">south</span> " <>
		"<span gravity=\"east\">east</span> " <>
		"<span gravity=\"north\">north</span> " <>
		"<span gravity=\"west\">west</span>" -- <>
--		"<span gravity=\"auto\">auto</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 710
	pangoLayoutSetMarkup pl $
		"<span gravity_hint=\"natural\">" <>
		"<span gravity=\"south\">south</span> " <>
		"<span gravity=\"east\">east</span> " <>
		"<span gravity=\"north\">north</span> " <>
		"<span gravity=\"west\">west</span> " <>
--		"<span gravity=\"auto\">auto</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 750
	pangoLayoutSetMarkup pl $
		"<span gravity_hint=\"strong\">" <>
		"<span gravity=\"south\">south</span> " <>
		"<span gravity=\"east\">east</span> " <>
		"<span gravity=\"north\">north</span> " <>
		"<span gravity=\"west\">west</span> " <>
--		"<span gravity=\"auto\">auto</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 790
	pangoLayoutSetMarkup pl $
		"<span gravity_hint=\"line\">" <>
		"<span gravity=\"south\">south</span> " <>
		"<span gravity=\"east\">east</span> " <>
		"<span gravity=\"north\">north</span> " <>
		"<span gravity=\"west\">west</span> " <>
--		"<span gravity=\"auto\">auto</span>" <>
		"</span>"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 830
	print =<< pangoLayoutSetMarkupWithAccel pl "foo bar _baz __baz" '_'
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-markup.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
