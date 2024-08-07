{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.KeySym
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.GdkDevice.GdkAxes
import Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures
import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkToplevelNew Nothing defaultGdkWindowAttr
	gdkWindowSetEventCompression w False
	gdkWindowSetTitle w "あいうえお"
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkKeyPress e -> do
			k <- gdkEventKey e
			case gdkEventKeyKeyval k of
				Xk_q -> pure False; _ -> pure True
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkMotionNotify e -> True <$ do
			m <- gdkEventMotion e
			let	d = gdkEventMotionDevice m
				as = gdkEventMotionAxes m
			print m
			print $ gdkEventMotionX m
			print $ gdkEventMotionY m
			print $ gdkEventMotionXRoot m
			print $ gdkEventMotionYRoot m
			putStrLn =<< gdkDeviceGetName (fromJust $ gdkEventMotionSourceDevice m)
			print =<< gdkDeviceGetAxis d as GdkAxisPressure
		e -> True <$ print e
