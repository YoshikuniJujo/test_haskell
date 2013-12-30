{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable, ForeignFunctionInterface #-}

module GdkDrawable (
	SomeGdkWindow(..),
	GdkWindow(..),

	gdkDrawPoint
) where

import Foreign.Ptr
import Foreign.C.Types

import GObject
import GdkGC

gClass "GObject" "GdkDrawable"
gClass "GdkDrawable" "GdkWindow"

foreign import ccall "gdk_draw_point" c_gdkDrawPoint ::
	Ptr GdkDrawable -> Ptr GdkGC -> CInt -> CInt -> IO ()

gdkDrawPoint :: GdkDrawable -> GdkGC -> Int -> Int -> IO ()
gdkDrawPoint gd gc x y = c_gdkDrawPoint
	(pointer gd) (pointer gc) (fromIntegral x) (fromIntegral y)
