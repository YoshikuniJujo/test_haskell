{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Primitive
import Data.Foldable
import Data.Char
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.Windows
import Graphics.Gdk.Events
import Graphics.Gdk.Cursors
import Try.Tools

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Paths
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Types
import Graphics.Cairo.Values

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkWindowNew Nothing defaultGdkWindowAttr
	d <- gdkWindowGetDisplay w
	st <- gdkDisplayGetDefaultSeat d
	pnt <- gdkSeatGetPointer st
	([], pds) <- gdkDeviceListSlaveDevices pnt
	for_ (zip pds ["wait", "cell", "crosshair", "text", "vertical-text"]) \(pd, nm) ->
		gdkWindowSetDeviceCursor w pd =<< gdkCursorNewFromName d nm
	gdkWindowShow w
	gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval k
			when (kv == fromIntegral (ord 'c'))
				$ gdkWindowSetCursor w =<< (\s -> gdkCursorNewFromSurface d s 15 15) =<< drawCursor
			when (kv == fromIntegral (ord 'd'))
				$ gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
			pure $ kv /= fromIntegral (ord 'q')
		e -> True <$ print e

drawCursor :: PrimMonad m => m (CairoSurfaceT (PrimState m))
drawCursor = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 50 50
	cr <- cairoCreate s
	cairoSetSourceRgb cr 0 1 0
	cairoSetLineWidth cr 3
	cairoMoveTo cr 15 15
	cairoLineTo cr 15 30
	cairoStroke cr
	cairoMoveTo cr 15 15
	cairoLineTo cr 30 15
	cairoStroke cr
	cairoMoveTo cr 15 15
	cairoLineTo cr 35 35
	cairoStroke cr
	pure s
