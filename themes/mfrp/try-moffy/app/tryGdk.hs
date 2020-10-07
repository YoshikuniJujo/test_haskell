{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Marshal hiding (void)
import Foreign.Storable

import Control.Monad
import Control.Concurrent
import System.Environment
import Graphics.Gdk
import Graphics.Gdk.Event
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
	c <- alloca $ \p -> do
		attr <- peek p
		gdkWindowAttrSetWindowType attr gdkWindowToplevel
--		gdkWindowAttrSetWindowType attr gdkWindowChild
		gdkWindowAttrSetX attr 100
		gdkWindowAttrSetY attr 100
		gdkWindowAttrSetWidth attr 200
		gdkWindowAttrSetHeight attr 200
		gdkWindowAttrSetWClass attr gdkInputOutput
--		gdkWindowNew Nothing attr [gdkWaWmclass]
		gdkWindowNew (Just w) attr [gdkWaWmclass, gdkWaX, gdkWaY]
	gdkWindowShow c
	gdkWithEvent $ maybe (pure ()) checkEvent
	gdkWithEvent $ maybe (pure ()) checkEvent
	gdkWithEvent $ maybe (pure ()) checkEvent
	gdkWithEvent $ maybe (pure ()) checkEvent
	gdkWithEvent $ maybe (pure ()) checkEvent
	gdkWithEvent $ maybe (pure ()) checkEvent
	gdkWithEvent $ maybe (pure ()) checkEvent
	gdkWithEvent $ maybe (pure ()) checkEvent
	gdkWithEvent $ maybe (pure ()) checkEvent
	gdkWithEvent $ maybe (pure ()) checkEvent
	do
		threadDelay 100000
		cairoRegionWithRectangle (CairoRectangleIntT 50 50 100 100) \r ->
			gdkWindowWithDrawFrame w r \cxt -> do
				cr <- gdkDrawingContextGetCairoContext cxt
				cairoSetSourceRgb cr 0.8 0.2 0.2
				cairoSetLineWidth cr 5
				cairoMoveTo cr 10 10
				cairoLineTo cr 90 90
				cairoStroke cr
		gdkWithEvent $ maybe (pure ()) checkEvent
	do
		threadDelay 100000
		cairoRegionWithRectangle (CairoRectangleIntT 50 50 100 100) \r ->
			gdkWindowWithDrawFrame c r \cxt -> do
				cr <- gdkDrawingContextGetCairoContext cxt
				cairoSetSourceRgb cr 0.2 0.8 0.2
				cairoSetLineWidth cr 5
				cairoMoveTo cr 10 90
				cairoLineTo cr 90 10
				cairoStroke cr
		gdkWithEvent $ maybe (pure ()) checkEvent
	getChar
	pure ()

checkEvent :: GdkEvent -> IO ()
checkEvent = \case
	GdkEventGdkEventConfigure c -> do
		print c
		print =<< gdkEventConfigureWidth c
		print =<< gdkEventConfigureHeight c
	e -> print e
