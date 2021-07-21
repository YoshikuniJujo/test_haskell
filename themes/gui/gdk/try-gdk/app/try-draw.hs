{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.Char
import System.Environment
import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.GdkDrawingContext
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Graphics.Gdk.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Regions
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Utilities.Types

import Data.Bool

import Data.Color

main :: IO ()
main = do
	_ <- join $ gdkInit <$> getProgName <*> getArgs
	d <- gdkDisplayGetDefault

	let wattr = minimalGdkWindowAttr (gdkEventMaskMultiBits [
				GdkExposureMask, GdkButtonPressMask, GdkKeyPressMask, GdkFocusChangeMask,
				GdkEnterNotifyMask, GdkLeaveNotifyMask, GdkPointerMotionMask,
--				GdkAllEventsMask,
				GdkPointerMotionMask
				])
			400 400
			gdkInputOutput GdkWindowToplevel
	w <- gdkWindowNew Nothing wattr { gdkWindowAttrTitle = Just "試験窓" }
	gdkWindowShow w
	gdkWindowSetEvents w $ gdkEventMaskMultiBits [
		GdkExposureMask, GdkButtonPressMask, GdkFocusChangeMask, GdkKeyPressMask,
		GdkEnterNotifyMask, GdkLeaveNotifyMask,
		GdkPointerMotionMask ] -- , GdkAllEventsMask ]
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkWithEventGet \case
			Just e -> do
				b <- checkEvent e
				pure if b then Nothing else Just False
			Nothing -> pure $ Just True
	gdkWindowDestroy w
	gdkDisplayClose d

checkEvent :: GdkEvent s -> IO Bool
checkEvent = \case
	GdkEventGdkKeyPress k -> do
		let	kv = gdkEventKeyKeyval $ gdkEventKey k
		pure $ kv /= GdkKeySym (fromIntegral $ ord 'q')
	GdkEventGdkVisibilityNotify (gdkEventVisibility -> v) -> True <$ print v
	GdkEventGdkEnterNotify (gdkEventCrossing -> e) -> True <$ putStrLn ("ENTER: " ++ show e)
	GdkEventGdkLeaveNotify (gdkEventCrossing -> l) -> True <$ putStrLn ("LEAVE: " ++ show l)
	GdkEventGdkMap m -> do
		putStrLn $ "GDK_MAP: " ++ show m
		drawRedLine $ tryGdkEventSealedMapWindow m
		pure True
	GdkEventSealedGdkConfigure c -> do
		print c
		drawRedLine $ tryGdkEventSealedConfigureWindow c
		pure True
	GdkEventSealedGdkFocusChange f -> True <$ print f
	GdkEventSealedGdkWindowState s -> True <$ print s
	GdkEventGdkAny a -> True <$ print a

drawRedLine :: GdkWindow -> IO ()
drawRedLine w = do
	r <- cairoRegionCreateRectangle $ CairoRectangleIntT 50 50 100 100
	gdkWindowWithDrawFrame w r \cxt -> do
		cr <- gdkDrawingContextGetCairoContext cxt
		cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.2
		cairoSetLineWidth cr 5
		cairoMoveTo cr 10 10
		cairoLineTo cr 90 90
		cairoStroke cr

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = bool (pure ()) (doWhile_ act) =<< act

doWhile :: Monad m => m (Maybe a) -> m a
doWhile act = maybe (doWhile act) pure =<< act
