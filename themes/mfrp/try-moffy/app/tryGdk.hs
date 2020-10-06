{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Marshal
import Foreign.Storable

import Control.Monad
import Control.Concurrent
import System.Environment
import Graphics.Gdk
import Graphics.Cairo

main :: IO ()
main = do
	as <- getArgs
	print =<< gdkInit as
	w <- alloca $ \p -> do
		attr <- peek p
		gdkWindowAttrSetWindowType attr gdkWindowToplevel
		gdkWindowAttrSetWidth attr 400
		gdkWindowAttrSetHeight attr 400
		gdkWindowAttrSetWClass attr gdkInputOutput
		gdkWindowNew Nothing attr [gdkWaWmclass]
	gdkWindowShow w
	print =<< gdkEventGet
	print =<< gdkEventGet
	print =<< gdkEventGet
	print =<< gdkEventGet
	print =<< gdkEventGet
	print =<< gdkEventGet
	print =<< gdkEventGet
	print =<< gdkEventGet
	print =<< gdkEventGet
	print =<< gdkEventGet
	print =<< gdkEventGet
	forever do
		threadDelay 100000
		cairoRegionWithRectangle (CairoRectangleIntT 50 50 100 100) \r ->
			gdkWindowWithDrawFrame w r \cxt -> do
				cr <- gdkDrawingContextGetCairoContext cxt
				cairoSetSourceRgb cr 0.8 0.2 0.2
				cairoSetLineWidth cr 5
				cairoMoveTo cr 10 10
				cairoLineTo cr 90 90
				cairoStroke cr
	getChar
	pure ()
