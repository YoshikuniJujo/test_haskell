{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Gtk
import Graphics.Cairo
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
