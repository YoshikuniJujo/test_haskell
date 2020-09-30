{-# LANGUAGE BlockArguments, TupleSections, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Concurrent.STM
import System.IO
import System.Environment
import Network.HTTP.Simple

import qualified Data.ByteString as BS

import Graphics.Gtk
import Graphics.Cairo
import Graphics.CairoType

main :: IO ()
main = do
	[pngFile] <- gtkInit =<< getArgs
	h <- openFile pngFile ReadMode
	bs <- getResponseBody <$> httpBS "https://upload.wikimedia.org/wikipedia/commons/1/17/PNG-Gradient_hex.png"
	tbs <- atomically $ newTVar bs
	cairoWithImageSurfaceFromPngStream (bsToCairoReadFunc tbs) () \png -> do
--	cairoWithImageSurfaceFromPngStream (readPngFunc h) () \png -> do
--	cairoWithImageSurfaceFromPng pngFile \png -> do
		w <- gtkWindowNew gtkWindowToplevel
		gSignalConnect (castGtkWidgetToGObject w) Destroy gtkMainQuit ()

		da <- gtkDrawingAreaNew
		gSignalConnect (castGtkWidgetToGObject da) DrawEvent (draw png) ()
		gtkContainerAdd (castWidgetToContainer w) da

		gtkWidgetShowAll w
		gtkMain

draw :: CairoSurfaceT -> GtkWidget -> CairoT -> () -> IO Bool
draw s _ cr () = True <$ do
	cairoScale cr (1 / 3) (1 / 3)
	cairoSetSourceSurface cr s 50 150
	cairoPaint cr
	cairoIdentityMatrix cr
	cairoScale cr (1 / 2) (1 / 2)
	cairoSetSourceSurface cr s 600 150
	cairoPaint cr

readPngFunc :: Handle -> CairoReadFunc ()
readPngFunc h () n = do
	print n
	(cairoStatusSuccess ,) . Just <$> BS.hGet h (fromIntegral n)
--	(cairoStatusReadError ,) . Just <$> BS.hGet h (fromIntegral n)

bsToCairoReadFunc :: TVar BS.ByteString -> CairoReadFunc ()
bsToCairoReadFunc tbs () (fromIntegral -> n) = atomically $ readTVar tbs >>= \bs -> case () of
	_	| BS.length bs >= n -> (cairoStatusSuccess, Just t) <$ writeTVar tbs d
		| otherwise -> pure (cairoStatusReadError, Nothing)
		where (t, d) = BS.splitAt n bs
