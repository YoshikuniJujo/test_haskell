{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent
import System.Environment
import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.GdkDrawingContext
import Graphics.Gdk.Event
import Graphics.Gdk.Types
import Graphics.Gdk.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Regions
import Graphics.Cairo.Paths
import Graphics.Cairo.Types

import Data.Bool

main :: IO ()
main = do
	pn <- getProgName
	as <- getArgs
	gdkSetAllowedBackends "win32,x11,*"
	print =<< gdkInit pn as
	print =<< gdkGetDisplayArgName
	gdkSetProgramClass "Foo"
	print =<< gdkGetProgramClass
	let wattr = mkGdkWindowAttr
			[gdkExposureMask, gdkButtonPressMask] 400 400
			gdkInputOutput gdkWindowToplevel
	w <- gdkWindowNew Nothing wattr { gdkWindowAttrTitle = Just "試験窓" }
	print =<< gdkWindowGetWindowType w
	print gdkWindowToplevel
	gdkWindowShow w
	gdkWindowSetEvents w [gdkExposureMask, gdkButtonPressMask]
	print gdkExposureMask
	gdkWindowInvalidateRect w (50, 50) (100, 100) False
	gdkWindowFreezeUpdates w
	gdkWindowThawUpdates w
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkEventGet >>= \case
			Just e -> do
				b <- checkEvent e
				pure if b then Nothing else Just False
			Nothing -> pure $ Just True

checkEvent :: GdkEvent -> IO Bool
checkEvent = \case
	GdkEventGdkNothing n -> do
		putStrLn $ "GDK_NOTHING: " ++ show n
		pure True
	GdkEventGdkDelete d -> do
		putStrLn $ "GDK_DELETE: " ++ show d
		pure False
	GdkEventGdkKeyPress k -> do
		kv <- gdkEventKeyKeyval k
		putStrLn $ "GDK_KEY_PRESS: " ++ show k ++ ": " ++ show kv
		pure True
	GdkEventGdkKeyRelease k -> do
		kv <- gdkEventKeyKeyval k
		putStrLn $ "GDK_KEY_RELEASE: " ++ show k ++ ": " ++ show kv
		pure True
	GdkEventGdkFocusChange f -> do
		i <- gdkEventFocusIn f
		putStrLn $ "GDK_FOCUS_CHANGE: " ++ show f ++ ": " ++ show i
		pure True
	GdkEventGdkMap m -> do
		putStrLn $ "GDK_MAP: " ++ show m
		pure True
	GdkEventGdkUnmap m -> do
		putStrLn $ "GDK_UNMAP: " ++ show m
		pure True
	GdkEventGdkConfigure c -> do
		w <- gdkEventConfigureWidth c
		h <- gdkEventConfigureHeight c
		putStrLn $ "GDK_CONFIGURE: " ++ show c ++ ": " ++ show w ++ " " ++ show h
		pure True
	GdkEventGdkVisibilityNotify v -> do
		w <- gdkEventVisibilityWindow v
		vs <- gdkEventVisibilityState v
		r <- cairoRegionCreateRectangle $ CairoRectangleIntT 50 50 100 100
		do
			gdkWindowWithDrawFrame w r \cxt -> do
				cr <- gdkDrawingContextGetCairoContext cxt
				cairoSetSourceRgb cr 0.8 0.2 0.2
				cairoSetLineWidth cr 5
				cairoMoveTo cr 10 10
				cairoLineTo cr 90 90
				cairoStroke cr
		putStrLn $ "GDK_VISIBILITY_NOTIFY: " ++ show v ++ ": " ++ show vs
		pure True
	GdkEventGdkWindowState s -> do
		ns <- gdkEventWindowStateNewWindowState s
		putStrLn $ "GDK_WINDOW_STATE: " ++ show s ++ ": " ++ show ns
		pure True
	e -> do	print e
		pure True

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = bool (pure ()) (doWhile_ act) =<< act

doWhile :: Monad m => m (Maybe a) -> m a
doWhile act = maybe (doWhile act) pure =<< act
