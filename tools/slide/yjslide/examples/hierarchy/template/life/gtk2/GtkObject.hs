{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkObject (
	gtkObjectToGObject,
	gtkObjectFromGObject,
	module GObject,
) where

import GObject

gClass "GObject" "GtkObject"
