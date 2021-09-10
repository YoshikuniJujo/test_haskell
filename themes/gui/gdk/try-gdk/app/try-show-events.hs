{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.KeySym
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures

import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkToplevelNew Nothing defaultGdkWindowAttr {
		gdkWindowAttrEventMask = gdkEventMaskMultiBits [
			GdkKeyPressMask,
			GdkButtonPressMask, GdkButtonReleaseMask,
			GdkScrollMask, GdkTouchMask, GdkSmoothScrollMask ] }
	gdkWindowShow w
	gdkSetShowEvents True
	print =<< gdkGetShowEvents
	mainLoop \case
		GdkEventGdkKeyPress e -> do
			k <- gdkEventKey e
			case gdkEventKeyKeyval k of
				Xk_q -> pure False; _ -> pure True
		GdkEventGdkAny _ -> pure True
