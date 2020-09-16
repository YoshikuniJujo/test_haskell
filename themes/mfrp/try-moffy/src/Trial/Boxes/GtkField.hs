{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.GtkField (
	-- * Box
	Box(..), Rect(..), BColor(..) ) where

import Control.Moffy.Handle.GtkField
import Control.Moffy.Viewable.Shape

import Graphics.Gtk.Cairo

instance Drawable Box where
	draw _ cr (Box (Rect (l_, u_) (r, d)) c) = do
		uncurry3 (cairoSetSourceRgb cr) $ colorToRgb c
		cairoRectangle cr l u w h
		cairoStrokePreserve cr
		cairoFill cr
		where
		l = min l_ r
		u = min u_ d
		w = abs $ l_ - r
		h = abs $ u_ - d

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

colorToRgb :: BColor -> (Double, Double, Double)
colorToRgb Red = (1, 0, 0)
colorToRgb Green = (0, 1, 0)
colorToRgb Blue = (0, 0, 1)
colorToRgb Yellow = (1, 1, 0)
colorToRgb Cyan = (0, 1, 1)
colorToRgb Magenta = (1, 0, 1)
