{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.GdkDevice.GdkAxes
import Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkWindowNew Nothing defaultGdkWindowAttr
	gdkWindowSetEventCompression w False
	gdkWindowSetTitle w "あいうえお"
	gdkWindowShow w
	mainLoopNew \case
		GdkEventSealedGdkKeyPress k -> case gdkEventKey k of
			GdkEventKey { gdkEventKeyKeyval = GdkKey_q } -> pure False
			_ -> pure True
		GdkEventSealedGdkDelete _d -> pure False
		GdkEventSealedGdkMotionNotify (gdkEventMotion -> m) -> True <$ do
			let	d = gdkEventMotionDevice m
				as = gdkEventMotionAxes m
			print m
			print $ gdkEventMotionX m
			print $ gdkEventMotionY m
			print $ gdkEventMotionXRoot m
			print $ gdkEventMotionYRoot m
			print =<< gdkDeviceGetAxis d as GdkAxisPressure
		e -> True <$ print e
