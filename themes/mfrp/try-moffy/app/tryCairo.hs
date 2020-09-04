{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.IO
import System.Environment

import qualified Data.ByteString as BS

import Graphics.Gtk
import Graphics.Gtk.Cairo
import Graphics.Gtk.Cairo.Values

main :: IO ()
main = do
	[pngFile] <- gtkInit =<< getArgs
	h <- openFile pngFile ReadMode
	cairoWithImageSurfaceFromPngStream (readPngFunc h) () \png -> do
--	cairoWithImageSurfaceFromPng pngFile \png -> do
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

readPngFunc :: Handle -> CairoReadFunc ()
readPngFunc h () n = do
	print n
	(cairoStatusSuccess ,) . Just <$> BS.hGet h (fromIntegral n)
--	(cairoStatusReadError ,) . Just <$> BS.hGet h (fromIntegral n)
