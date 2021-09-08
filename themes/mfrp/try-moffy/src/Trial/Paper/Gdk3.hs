{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Paper.Gdk3 where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Gdk.Windows
import Graphics.Gdk.GdkDrawingContext

import Trial.Paper

showRect :: GdkWindow -> Rect -> IO ()
showRect w (Rect (l_, u_) (r_, d_)) = do
	let (l, u, r, d) = lurd l_ u_ r_ d_
	rgn <- gdkWindowGetVisibleRegion w
	gdkWindowWithDrawFrame w rgn \ctx -> do
		cr <- gdkDrawingContextGetCairoContext ctx
		cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0.5 0
		cairoRectangle cr l u (r - l) (d - u)
		cairoFill cr
	where lurd l u r d = (
		realToFrac $ l `min` r, realToFrac $ u `min` d,
		realToFrac $ l `max` r, realToFrac $ u `max` d )
