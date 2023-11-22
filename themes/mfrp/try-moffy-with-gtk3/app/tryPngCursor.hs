{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Gtk
import Graphics.Cairo

import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gSignalConnect (castGtkWidgetToGObject w) Destroy gtkMainQuit ()

	cairoWithImageSurfaceFromPng fp \s -> do
		gtkWidgetShowAll w

		dw <- gtkWidgetGetWindow w
		dd <- gdkWindowGetDisplay dw
		csr <- gdkCursorNewFromSurface dd s 0 0
		gdkWindowSetCursor dw csr

		gtkMain
