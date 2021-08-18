{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Try.Tools

main :: IO ()
main = do
	_dpy <- gdkDisplayOpen ""
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [
			GdkKeyPressMask
			])
		900 700 GdkInputOutput GdkWindowToplevel
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) -> pure False
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> c) -> True <$ do
				let	(dx, dy) = case c of
						GdkKey_h -> (- 1, 0)
						GdkKey_j -> (0, 1)
						GdkKey_k -> (0, - 1)
						GdkKey_l -> (1, 0)
						_ -> (0, 0)
				(x, y) <- gdkWindowGetPosition w
				gdkWindowMove w (x + dx) (y + dy)
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e
