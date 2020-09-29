{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Gtk
import Graphics.Gtk.Cairo
import Graphics.Gtk.Cairo.Values

import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gSignalConnect w Destroy gtkMainQuit ()

	cairoWithImageSurfaceFromPng fp \s -> do
		gtkWidgetShowAll w

		dw <- gtkWidgetGetWindow w
		dd <- gdkWindowGetDisplay dw
		csr <- gdkCursorNewFromSurface dd s 0 0
		gdkWindowSetCursor dw csr

		gtkMain
