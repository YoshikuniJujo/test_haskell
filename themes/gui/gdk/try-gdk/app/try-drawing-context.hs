{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.GdkDrawingContext
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Regions
import Graphics.Cairo.Utilities.Types

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
				print r
				r' <- gdkDrawingContextGetClip cxt
				print r'
				print =<< cairoRegionNumRectangles r
				print =<< cairoRegionNumRectangles r'
				rct <- cairoRectangleIntTNew
				cairoRegionGetRectangle r 0 rct
				print =<< cairoRectangleIntTFreeze rct
				cairoRegionGetRectangle r' 0 rct
				print =<< cairoRectangleIntTFreeze rct
				cr <- gdkDrawingContextGetCairoContext cxt
				cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0.5 0
				cairoPaint cr
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
			pure False
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e
