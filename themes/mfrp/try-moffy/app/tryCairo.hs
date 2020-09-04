{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Environment

import Graphics.Gtk
import Graphics.Gtk.Cairo

main :: IO ()
main = do
	[pngFile] <- gtkInit =<< getArgs
	cairoWithImageSurfaceFromPng pngFile \png -> do
		w <- gtkWindowNew gtkWindowToplevel
		gSignalConnect w Destroy gtkMainQuit ()

		da <- gtkDrawingAreaNew
		gSignalConnect da DrawEvent draw png
		gtkContainerAdd (castWidgetToContainer w) da

		gtkWidgetShowAll w
		gtkMain

draw :: GtkWidget -> CairoT -> CairoSurfaceT -> IO Bool
draw _ cr s = True <$ do
	cairoSetSourceSurface cr s 50 50
	cairoPaint cr
