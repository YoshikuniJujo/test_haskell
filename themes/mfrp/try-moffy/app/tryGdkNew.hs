{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM (atomically, TVar, readTVar)
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Event.Window
import Control.Moffy.Viewable.Shape
import Data.Type.Flip
import Data.Map (Map, (!))
import Data.Maybe
import Data.Color
import Trial.TryGdk

import Graphics.Cairo.Drawing.CairoT

import Graphics.Gdk.Windows
import Graphics.Gdk.GdkDrawingContext

import Trial.Boxes

main :: IO ()
main = do
	(wid, i2w, w2i, t) <- initialize
	(print =<<) . ($ initTryGdkState t) $ interpretSt @_ @TryGdkEv
		(handle wid i2w w2i) (uncurry $ showColor i2w) do
		w <- adjustSig defaultWindowNew
		(w ,) <$%> adjustSig cycleColor

showColor :: TVar (Map WindowId GdkWindow) -> WindowId -> BColor -> IO ()
showColor i2w i bc = do
	w <- (! i) <$> atomically (readTVar i2w)
	r <- gdkWindowGetVisibleRegion w
	gdkWindowWithDrawFrame w r \ctx -> do
		cr <- gdkDrawingContextGetCairoContext ctx
		cairoSetSourceRgb cr $ bColorToRgb bc
		cairoPaint cr

bColorToRgb :: BColor -> Rgb
bColorToRgb Red = fromJust $ rgbDouble 0.5 0 0
bColorToRgb Green = fromJust $ rgbDouble 0 0.5 0
bColorToRgb Blue = fromJust $ rgbDouble 0 0 0.5
bColorToRgb Yellow = fromJust $ rgbDouble 0.5 0.5 0
bColorToRgb Cyan = fromJust $ rgbDouble 0 0.5 0.5
bColorToRgb Magenta = fromJust $ rgbDouble 0.5 0 0.5
