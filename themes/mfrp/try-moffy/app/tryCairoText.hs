{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Gtk
import Graphics.Cairo
import Graphics.Cairo.Values

main :: IO ()
main = do
	gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gSignalConnect w Destroy gtkMainQuit ()

	da <- gtkDrawingAreaNew
	gSignalConnect da DrawEvent draw ()
	gtkContainerAdd (castWidgetToContainer w) da

	gtkWidgetShowAll w

	dw <- gtkWidgetGetWindow w
	dd <- gdkWindowGetDisplay dw
	csr <- gdkCursorNewFromName dd "wait"
	maybe (pure ()) (gdkWindowSetCursor dw) csr

	gtkMain

draw :: GtkWidget -> CairoT -> a -> IO Bool
draw w cr _ = True <$ do
	cairoMoveTo cr 100 100
	cairoLineTo cr 500 400
	cairoStroke cr
	cairoMoveTo cr 200 120
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
