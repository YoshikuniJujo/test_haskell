{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkDrawingArea (
	gtkDrawingAreaNew
) where

import Control.Applicative
import Foreign.Ptr
import GtkWidget

gClass "GtkWidget" "GtkDrawingArea"

foreign import ccall "gtk_drawing_area_new" c_gtkDrawingAreaNew ::
	IO (Ptr SomeGtkDrawingArea)

gtkDrawingAreaNew :: IO SomeGtkDrawingArea
gtkDrawingAreaNew = SomeGtkDrawingArea <$> c_gtkDrawingAreaNew
