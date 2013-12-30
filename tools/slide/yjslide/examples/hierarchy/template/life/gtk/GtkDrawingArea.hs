{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkDrawingArea (
	SomeGtkDrawingArea(..)
) where

import GObject
import GtkWidget

gClass "GtkWidget" "GtkDrawingArea"
