{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Trial.Followbox.GtkField (
	-- * VIEW
	View(..), View1,
	-- * COLOR
	Color(..), white, blue, Png(..),
	-- * TEMP
	VText(..), Line(..), Image(..)
	) where

import Control.Moffy.Handle.GtkField
import Control.Moffy.View.GtkField
import Data.OneOfThem
import Graphics.Gtk
import Graphics.Gtk.Cairo

import Trial.Followbox.ViewType

---------------------------------------------------------------------------

instance Drawable View where
	draw wdt cr (View v) = do
		w <- gtkWidgetGetAllocatedWidth wdt
		h <- gtkWidgetGetAllocatedHeight wdt
		print (w, h)
		cairoSetSourceRgb cr 0 0 0
		cairoRectangle cr 0 0 (fromIntegral w) (fromIntegral h)
		cairoStrokePreserve cr
		cairoFill cr
		((drawText wdt cr >-- drawLine wdt cr >-- SingletonFun (drawImage wdt cr)) `apply`) `mapM_` v
