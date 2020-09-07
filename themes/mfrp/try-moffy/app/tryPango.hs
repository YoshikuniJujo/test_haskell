{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Gtk
import Graphics.Gtk.Cairo
import Graphics.Gtk.Cairo.Values
import Graphics.Gtk.Pango

main :: IO ()
main = do
	gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gSignalConnect w Destroy gtkMainQuit ()

	da <- gtkDrawingAreaNew
	gSignalConnect da DrawEvent draw ()
	gtkContainerAdd (castWidgetToContainer w) da

	gtkWidgetShowAll w
	gtkMain

draw :: GtkWidget -> CairoT -> a -> IO Bool
draw w cr _ = True <$ do
	cairoMoveTo cr 100 100
	cairoLineTo cr 500 400
	cairoStroke cr
	l <- pangoCairoCreateLayout cr

	cairoMoveTo cr 200 120
	pangoLayoutSetText l "Hello, world!"
	pangoCairoShowLayout cr l
	cairoMoveTo cr 200 150
	pangoLayoutSetText l "こんにちは、世界!"
	pangoCairoShowLayout cr l

	sans <- pangoFontDescriptionFromString "Sans"

	pangoFontDescriptionSetAbsoluteSize sans 20
--	serif <- pangoFontDescriptionFromString "Serif"
	pangoLayoutSetFontDescription l sans
--	pangoLayoutSetFontDescription l serif
	cairoMoveTo cr 200 180
	pangoLayoutSetText l "Hello, world!"
	pangoCairoShowLayout cr l
	cairoMoveTo cr 200 210
	pangoLayoutSetText l "こんにちは、世界!"
	pangoCairoShowLayout cr l

	pangoFontDescriptionSetSize sans 20
--	serif <- pangoFontDescriptionFromString "Serif"
	pangoLayoutSetFontDescription l sans
--	pangoLayoutSetFontDescription l serif
	cairoMoveTo cr 200 240
	pangoLayoutSetText l "Hello, world!"
	pangoCairoShowLayout cr l
	cairoMoveTo cr 200 270
	pangoLayoutSetText l "こんにちは、世界!"
	pangoCairoShowLayout cr l

	{-
	cairoSelectFontFace cr "Serif" cairoFontSlantNormal cairoFontWeightNormal
	cairoSetFontSize cr 30
	cairoShowText cr "Hello, world! "
	cairoShowText cr "こんにちは、世界!"
	cairoMoveTo cr 200 160
	cairoSelectFontFace cr "Serif" cairoFontSlantNormal cairoFontWeightBold
	cairoSetFontSize cr 30
	cairoShowText cr "Hello, world! "
	cairoShowText cr "こんにちは、世界!"
	cairoMoveTo cr 200 200
	cairoSelectFontFace cr "Sans" cairoFontSlantNormal cairoFontWeightNormal
	cairoSetFontSize cr 30
	cairoShowText cr "Hello, world! "
	cairoShowText cr "こんにちは、世界!"
	cairoMoveTo cr 200 240
	cairoSelectFontFace cr "Sans" cairoFontSlantNormal cairoFontWeightBold
	cairoSetFontSize cr 30
	cairoShowText cr "Hello, world! "
	cairoShowText cr "こんにちは、世界!"
	-}
