{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad
import Control.Monad.Primitive
import Data.Char
import Data.KeySym
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkEventType
import Graphics.Gdk.Cursors
import Try.Tools

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Data.Color
import Data.Maybe

main :: IO ()
main = do
	print GdkKeyPress
	print GdkEnterNotify
	print GdkLeaveNotify
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkToplevelNew Nothing defaultGdkWindowAttr
	let	d = gdkWindowGetDisplay w
	st <- gdkDisplayGetDefaultSeat d
	pnt <- gdkSeatGetPointer st
	_pds <- gdkDeviceListSlaveDevices pnt
	{-
	for_ (zip pds ["wait", "cell", "crosshair", "text", "vertical-text"]) \(pd, nm) ->
		gdkWindowSetDeviceCursor w pd =<< gdkCursorNewFromName d nm
		-}
	gdkWindowShow w
	gdkWindowSetEventCompression w False
	gdkWindowSetEvents w $ gdkEventMaskMultiBits [
		GdkPointerMotionMask, GdkButtonPressMask,
		GdkButtonReleaseMask, GdkKeyPressMask ] -- , gdkAllEventsMask]
	gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkMotionNotify m -> True <$ do
			putStr "GDK_MOTION_NOTIFY: "
			print =<< gdkEventMotionPos m
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval <$> gdkEventKey k
			when (kv == KeySym (fromIntegral $ ord 'c'))
--				$ gdkWindowSetCursor w =<< (\s -> gdkCursorNewFromSurface d s 15 15) =<< drawCursor
				$ gdkWindowSetCursor w =<< (\s -> getSurfaceCursor d s 15 15) =<< drawCursor
			when (kv == KeySym (fromIntegral $ ord 'd'))
--				$ gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
				$ gdkWindowSetCursor w =<< getNameCursor d "crosshair"
			when (kv == KeySym (fromIntegral $ ord 'g'))
				$ print =<< gdkSeatGrab st w GdkSeatCapabilityAllPointing False Nothing Nothing
						(Nothing :: Maybe (GdkSeatGrabPrepareFunc (), ()))
			when (kv == KeySym (fromIntegral $ ord 'h'))
				$ print =<< gdkSeatGrab st w GdkSeatCapabilityAllPointing True Nothing Nothing
						(Nothing :: Maybe (GdkSeatGrabPrepareFunc (), ()))
			pure $ kv /= KeySym (fromIntegral $ ord 'q')
		e -> True <$ print e

getSurfaceCursor :: GdkDisplay -> CairoSurfaceImageT s ps -> CDouble -> CDouble -> IO GdkCursor
getSurfaceCursor d si x y = do
	c <- gdkCursorNewFromSurface d si x y
	pure c

getNameCursor :: GdkDisplay -> String -> IO GdkCursor
getNameCursor d n = do
	c <- gdkCursorNewFromName d n
	pure c

drawCursor :: PrimMonad m => m (CairoSurfaceImageT s (PrimState m))
drawCursor = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 50 50
	cr <- cairoCreate s
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 1 0
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

gdkEventMotionPos :: Sealed s GdkEventMotionRaw -> IO (CDouble, CDouble)
gdkEventMotionPos m_ = do
	m <- gdkEventMotion m_
	pure (gdkEventMotionX m, gdkEventMotionY m)
