{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Primitive
import Data.Char
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.Windows
import Graphics.Gdk.Events
import Graphics.Gdk.Cursors
import Graphics.Gdk.Values
import Try.Tools

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Data.Color
import Data.Maybe

main :: IO ()
main = do
	print gdkKeyPress
	print gdkEnterNotify
	print gdkLeaveNotify
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkWindowNew Nothing defaultGdkWindowAttr
	d <- gdkWindowGetDisplay w
	st <- gdkDisplayGetDefaultSeat d
	pnt <- gdkSeatGetPointer st
	([], _pds) <- gdkDeviceListSlaveDevices pnt
	{-
	for_ (zip pds ["wait", "cell", "crosshair", "text", "vertical-text"]) \(pd, nm) ->
		gdkWindowSetDeviceCursor w pd =<< gdkCursorNewFromName d nm
		-}
	gdkWindowShow w
	gdkWindowSetEventCompression w False
	gdkWindowSetEvents w [gdkPointerMotionMask, gdkButtonPressMask, gdkButtonReleaseMask, gdkKeyPressMask] -- , gdkAllEventsMask]
	gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		e@(GdkEventGdkMotionNotify m) -> True <$ do
			putStr "GDK_MOTION_NOTIFY: "
			print =<< gdkEventMotionPos m
			sd <- gdkEventGetSourceDevice e
			print sd
			putStrLn =<< maybe (pure "No source device") gdkDeviceGetName sd
--			putStrLn =<< maybe (pure "No device tool") ((show <$>) . gdkDeviceToolGetToolType)
--				=<< gdkEventGetDeviceTool e
			mis <- maybe (pure Nothing) ((Just <$>) . gdkDeviceGetSource)  sd
			case mis of
				Nothing -> pure ()
				Just is	| is == gdkSourceMouse -> gdkWindowSetCursor w =<< gdkCursorNewFromName d "wait"
					| is == gdkSourcePen -> gdkWindowSetCursor w =<< gdkCursorNewFromName d "text"
					| is == gdkSourceTouchpad -> gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
					| otherwise -> pure ()
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval k
			when (kv == fromIntegral (ord 'c'))
				$ gdkWindowSetCursor w =<< (\s -> gdkCursorNewFromSurface d s 15 15) =<< drawCursor
			when (kv == fromIntegral (ord 'd'))
				$ gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
			when (kv == fromIntegral (ord 'g'))
				$ print =<< gdkSeatGrab st w gdkSeatCapabilityAllPointing False Nothing Nothing
						(Nothing :: Maybe (GdkSeatGrabPrepareFunc (), ()))
			when (kv == fromIntegral (ord 'h'))
				$ print =<< gdkSeatGrab st w gdkSeatCapabilityAllPointing True Nothing Nothing
						(Nothing :: Maybe (GdkSeatGrabPrepareFunc (), ()))
			pure $ kv /= fromIntegral (ord 'q')
		e -> True <$ print e

drawCursor :: PrimMonad m => m (CairoSurfaceT s (PrimState m))
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
	pure $ toCairoSurfaceT s

gdkEventMotionPos :: GdkEventMotion -> IO (Double, Double)
gdkEventMotionPos m = (,) <$> gdkEventMotionX m <*> gdkEventMotionY m
