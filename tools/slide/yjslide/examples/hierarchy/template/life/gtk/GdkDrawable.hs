{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable, ForeignFunctionInterface #-}

module GdkDrawable (
	SomeGdkWindow(..),
	GdkWindow(..),

	gdkDrawPoint,
	gdkDrawRectangle,
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

foreign import ccall "gdk_draw_rectangle" c_gdkDrawRectangle ::
	Ptr GdkDrawable -> Ptr GdkGC -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

gdkDrawRectangle :: GdkDrawable -> GdkGC -> Bool -> Int -> Int -> Int -> Int -> IO ()
gdkDrawRectangle gd gc filled x y width height = c_gdkDrawRectangle
	(pointer gd) (pointer gc) (fromIntegral $ fromEnum filled)
	(fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)
