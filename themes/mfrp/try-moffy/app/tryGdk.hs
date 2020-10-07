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
	getChar
	pure ()

checkEvent :: GdkEvent -> IO ()
checkEvent = \case
	GdkEventGdkMap m -> putStrLn $ "GDK_MAP: " ++ show m
	GdkEventGdkConfigure c -> do
		w <- gdkEventConfigureWidth c
		h <- gdkEventConfigureHeight c
		putStrLn $ "GDK_CONFIGURE: " ++ show c ++ ": " ++ show w ++ " " ++ show h
	GdkEventGdkVisibilityNotify v -> do
		vs <- gdkEventVisibilityState v
		putStrLn $ "GDK_VISIBILITY_NOTIFY: " ++ show v ++ ": " ++ show vs
	GdkEventGdkWindowState s -> do
		ns <- gdkEventWindowStateNewWindowState s
		putStrLn $ "GDK_WINDOW_STATE: " ++ show s ++ ": " ++ show ns
	e -> print e
