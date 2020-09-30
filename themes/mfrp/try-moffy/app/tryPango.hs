{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Gtk
import Graphics.Cairo
import Graphics.Pango
import Graphics.CairoType

main :: IO ()
main = do
	gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gSignalConnect (castGtkWidgetToGObject w) Destroy gtkMainQuit ()

	da <- gtkDrawingAreaNew
	gSignalConnect (castGtkWidgetToGObject da) DrawEvent draw ()
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
--	pangoLayoutWithExtents l \ie le -> do
	pangoLayoutWithPixelExtents l \ie le -> do
		print ie
		print =<< pangoRectangleX ie
		print =<< pangoRectangleY ie
		print =<< pangoRectangleWidth ie
		print =<< pangoRectangleHeight ie
		print le
		print =<< pangoRectangleX le
		print =<< pangoRectangleY le
		print =<< pangoRectangleWidth le
		print =<< pangoRectangleHeight le
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

	pangoLayoutSetText l "top left"
	cairoMoveTo cr 0 0
	pangoCairoShowLayout cr l
