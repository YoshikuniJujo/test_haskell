{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkWindowNew Nothing defaultGdkWindowAttr {
		gdkWindowAttrEventMask = gdkEventMaskMultiBits [
			GdkKeyPressMask,
			GdkButtonPressMask, GdkButtonReleaseMask,
			GdkScrollMask, GdkTouchMask, GdkSmoothScrollMask ] }
	gdkWindowShow w
	gdkSetShowEvents True
	print =<< gdkGetShowEvents
	mainLoop \case
		GdkEventGdkKeyPress k -> case gdkEventKey k of
			GdkEventKey { gdkEventKeyKeyval = GdkKey_q } -> pure False
			_ -> pure True
		GdkEventGdkAny _ -> pure True
