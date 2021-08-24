{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.GdkDrawingContext
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Try.Tools

main :: IO ()
main = do
	_ <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [GdkKeyPressMask, GdkFocusChangeMask])
		500 350
	gdkWindowShow win
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkFocusChange (gdkEventFocus -> f) -> True <$ do
			print f
			r <- gdkWindowGetVisibleRegion win
			gdkWindowWithDrawFrame win r \cxt -> do
				print cxt
				print =<< gdkDrawingContextIsValid cxt
				print win
				print =<< gdkDrawingContextGetWindow cxt
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
			pure False
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e
