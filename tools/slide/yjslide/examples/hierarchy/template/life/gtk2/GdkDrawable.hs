{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GdkDrawable (
	SomeGdkWindow(..),
	gdkCairoCreate,

	module Cairo,
) where

import Control.Applicative

import Foreign.Ptr

import GObject
import Cairo

gClass "GObject" "GdkDrawable"

foreign import ccall "gdk_cairo_create" c_gdkCairoCreate ::
	Ptr GdkDrawable -> IO (Ptr CairoT)
gdkCairoCreate :: GdkDrawable -> IO CairoT
gdkCairoCreate gd = CairoT <$> c_gdkCairoCreate (pointer gd)

gClass "GdkDrawable" "GdkWindow"
