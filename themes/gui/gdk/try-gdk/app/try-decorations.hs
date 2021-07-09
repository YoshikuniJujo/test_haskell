{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.Char
import Text.Read
import System.Environment

import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Regions
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Utilities.Types

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Events
import Graphics.Gdk.Values
import Graphics.Gdk.GdkDrawingContext

import Try.Tools

main :: IO ()
main = do
	(_pn, as) <- join $ gdkInit <$> getProgName <*> getArgs
	print as
	let	ds = catMaybes $ readMaybe <$> as
		ds' = gdkWMDecorations ds
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [GdkKeyPressMask])
		100 100 gdkInputOutput GdkWindowToplevel
	print ds
	gdkWindowSetDecorations w ds'
	gdkWindowShow w
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkEventGet >>= \case
			Just (GdkEventGdkMap m) -> do
				r <- cairoRegionCreateRectangle $ CairoRectangleIntT 0 0 100 100
				gdkWindowWithDrawFrame w r \cxt -> do
					cr <- gdkDrawingContextGetCairoContext cxt
					cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.2
					cairoSetLineWidth cr 5
					cairoMoveTo cr 10 10
					cairoLineTo cr 90 90
					cairoStroke cr
				pure Nothing
			Just (GdkEventGdkKeyPress k) -> do
				kv <- gdkEventKeyKeyval k
				pure . Just $ kv /= fromIntegral (ord 'q')
			Just _ -> pure Nothing
			Nothing -> pure $ Just True
