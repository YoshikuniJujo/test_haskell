{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkDrawingArea (
	SomeGtkDrawingArea(..),
	gtkDrawingAreaNew
) where

import Foreign.Ptr
import Control.Applicative

import GObject
import GtkWidget

gClass "GtkWidget" "GtkDrawingArea"

foreign import ccall "gtk_drawing_area_new" c_gtkDrawingAreaNew ::
	IO (Ptr SomeGtkDrawingArea)

gtkDrawingAreaNew :: IO SomeGtkDrawingArea
gtkDrawingAreaNew = SomeGtkDrawingArea <$> c_gtkDrawingAreaNew
