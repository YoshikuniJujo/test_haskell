{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkStyle (
	GtkStyle(..),
	SomeGtkStyle(..),
) where

import GObject

gClass "GObject" "GtkStyle"

-- c_fgGC :: Ptr GtkStyle -> Ptr (Ptr 
